/*
 *  Copyright (C) 2011 The Animo Project
 *  http://animotron.org
 *
 *  This program is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public License
 *  as published by the Free Software Foundation; either version 3
 *  of the License, or (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */
package org.animotron.graph;

import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.LinkedList;
import java.util.List;
import java.util.Stack;
import java.util.StringTokenizer;

import org.animotron.Properties;
import org.animotron.Statement;
import org.animotron.Statements;
import org.animotron.instruction.Instruction;
import org.animotron.instruction.InstructionContainer;
import org.animotron.instruction.ml.ATTRIBUTE;
import org.animotron.instruction.ml.CDATA;
import org.animotron.instruction.ml.COMMENT;
import org.animotron.instruction.ml.ELEMENT;
import org.animotron.instruction.ml.TEXT;
import org.animotron.operator.Evaluable;
import org.animotron.operator.External;
import org.animotron.operator.Operator;
import org.animotron.operator.Property;
import org.animotron.operator.Relation;
import org.animotron.operator.THE;
import org.exist.security.MessageDigester;
import org.neo4j.graphdb.Direction;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.Transaction;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * 
 */
public class AnimoGraphBuilder {
	
	private Relationship the = null;
	
	private static final String CACHE_ALGOTHIM = "SHA-256";
	
	private Transaction tx;
	private Stack<Object[]> statements;
		
	public Relationship getTHE() {
		return this.the;
	}
	
	private void setTHE(Relationship the) {
		this.the = the;
	}
	
	private MessageDigest md() {
		try {
			return MessageDigest.getInstance(CACHE_ALGOTHIM);
		} catch (NoSuchAlgorithmException e) {
			//can't be, but throw runtime error
			throw new RuntimeException(e);
		}
	}
	
	public void startDocument(){
		statements = new Stack<Object[]>();
		tx = AnimoGraph.beginTx();
		the = null;
	};
	
	public void startElement(String ns, String name) {
		
		Statement statement = Statements.namespace(ns);
		
		if (statement instanceof InstructionContainer) {
			statement = ((InstructionContainer) statement).getInstruction(name);
		}
		
		if (statement == null) 
			statement = ELEMENT.getInstance();
		
		start(statement, ns, name, null);
		
	}
	
	private void start(Statement statement, String ns, String name, String value) {
		
		Node the = null, target = null;
		
		MessageDigest md = md();
		
		if (statement instanceof THE){
			the = ((THE) statement).build(AnimoGraph.THE, name);
			
		}
		
		if (ns != null) 
			md.update(ns.getBytes());
		
		if (name != null) 
			md.update(name.getBytes());
		
		if (statement instanceof Instruction) 
			md.update(statement.name().getBytes());

		
		if (value != null) {
			byte[] b = value.getBytes(); 
			MessageDigest tmp = md();
			tmp.update(b);
			String hash = MessageDigester.byteArrayToHex(tmp.digest());
			target = THE.getInstance().node(AnimoGraph.CACHE, hash);
			if (target == null){
				target = THE.getInstance().create(AnimoGraph.CACHE, hash);
				Properties.VALUE.set(target, value);
			}
			md.update(b);
		}
		
		boolean external = statement instanceof External;
		
		if (!statements.empty()) 
			external |= (Boolean) statements.peek()[5];
		
		Object[] item = {statement, name, md, new LinkedList<Node>(), the, external, null, ns, target};
		
		statements.push(item);
		
	}
	
	public void endElement(String ns, String name) {
		end();
	}
	
	private void end(){
		
		Object[] currentItem = statements.pop();
		
		Statement currentStatement = (Statement) currentItem[0];
		
		MessageDigest md = (MessageDigest) currentItem[2];
		byte [] digest = md.digest();
		
		String name = (String) currentItem[1];
		String ns = (String) currentItem[7];
		
		
		try {
			
			if (currentStatement instanceof THE) {
				Node node = (Node) currentItem[4];
				addChildren(node, (List<Node>) currentItem[3]);
				return;
			}
			
			boolean isProperty = currentStatement instanceof Property;
			boolean isRelation = currentStatement instanceof Relation || 
								 currentStatement instanceof ATTRIBUTE ||
								 currentStatement instanceof TEXT ||
								 currentStatement instanceof COMMENT ||
								 currentStatement instanceof CDATA;
			boolean isCachable = !((isProperty || isRelation) && !statements.empty());
			
			if (!isCachable) {
				
				Node tmp;
				Object[] parentItem = statements.peek();
				Statement parentStatement = (Statement) parentItem[0];
				boolean isTHE = parentStatement instanceof THE;
				
				if (isTHE) {
					tmp = (Node) parentItem[4]; 
				} else {
					tmp = (Node) parentItem[6];
					if (tmp == null) {
						tmp = AnimoGraph.createNode();
						parentItem[6] = tmp;
					}
				}
				
				if (currentStatement instanceof Operator) {
					Node child = (Node) currentItem[6];
					Operator operator = (Operator) currentStatement;
					Node res = child != null ? operator.build(tmp, child, name) : operator.build(tmp, name);
					if (isProperty)
						addChildren(res, (List<Node>) currentItem[3]);
					
				} else if (currentStatement instanceof ATTRIBUTE) {
					Node target = (Node) currentItem[8];
					ATTRIBUTE attribute = (ATTRIBUTE) currentStatement;
					attribute.build(tmp, target, ns, name);
					
				} else {
					Node target = (Node) currentItem[8];
					Instruction instruction = (Instruction) currentStatement;
					instruction.build(tmp, target);

				}
				
				if (isTHE)
					return;
				
			} else {
				
				String hash = MessageDigester.byteArrayToHex(digest);
				THE the = THE.getInstance();
				Node cache = the.node(AnimoGraph.CACHE, hash);
				
				if (cache == null) {
					
					cache = the.create(AnimoGraph.CACHE, hash);
					
					if (currentStatement instanceof Operator) {
						Operator operator = (Operator) currentStatement;
						Node tmp = (Node) currentItem[6];
						Node node = tmp != null ? operator.build(cache, tmp, name) : operator.build(cache, name);
						addChildren(node, (List<Node>) currentItem[3]);
			
					} else if (currentStatement instanceof ELEMENT) {
						ELEMENT element = ELEMENT.getInstance();
						Node tmp = (Node) currentItem[6];
						Node node = tmp != null ? element.build(cache, tmp, ns, name) : element.build(cache, ns, name); 
						addChildren(node, (List<Node>) currentItem[3]);
						
					} else {
						Instruction instruction = (Instruction) currentStatement;
						Node tmp = (Node) currentItem[6];
						Node node = tmp != null ? instruction.build(cache, tmp) : instruction.build(cache);
						addChildren(node, (List<Node>) currentItem[3]);
						
					}
					
					boolean external = false; 
					
					if (!statements.empty()) {
						external = (Boolean) statements.peek()[5];
					}

					if (currentStatement instanceof Evaluable && !external){
						AnimoGraph.CALC.createRelationshipTo(cache, RelationshipTypes.CALCULATE);
					}
					
				}
				
				if (!statements.empty()) {
					((List<Node>) statements.peek()[3]).add(cache);
				}
				
			}
			
			if (!statements.empty()) {
				((MessageDigest) statements.peek()[2]).update(digest); 
			}
			
		} catch (Exception e){
			System.out.println("Error building element " + currentStatement.name());
			e.printStackTrace(System.out);
			tx.finish();
		}
		
	}

	public void attribute(String ns, String name, String value) {
		start(ATTRIBUTE.getInstance(), ns, name, value);
		end();
	}

	public void text (String text) {
		
		StringBuilder buf = new StringBuilder();
		if (text.length() > 0) {
			StringTokenizer tok = new StringTokenizer(text);
			while (tok.hasMoreTokens()) {
                buf.append(tok.nextToken());
				if (tok.hasMoreTokens()) buf.append(' ');
			}
		}
		
		if (buf.length() > 0) {
			start(TEXT.getInstance(), null, null, buf.toString());
			end();
		}
			
	}
		
	public void comment(String text) {
		start(COMMENT.getInstance(), null, null, text);
		end();
	}
	
	public void cdata (String text) {
		start(CDATA.getInstance(), null, null, text);
		end();
	}
	
	public void endDocument(){
		tx.success();
		tx.finish();
	}

	private void addChildren(Node node, List<Node> children) {
		for (Node n : children) {
			for (Relationship r : n.getRelationships(Direction.OUTGOING)){
				node.createRelationshipTo(r.getEndNode(), r.getType());
			}
		}
	}
	
}
