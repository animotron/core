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
import org.animotron.instruction.ml.MLInstruction;
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
		
		MessageDigest md = md();
		
		if (statement == null) {
			statement = ELEMENT.getInstance(); 
			md.update(statement.namespace().getBytes());
			md.update(statement.name().getBytes());
		}

		md.update(ns.getBytes());
		md.update(name.getBytes());
		
		Node the = null;
		//move the instance to GC & create new
		if (statement instanceof THE){
			the = ((THE) statement).build(AnimoGraph.THE, name);
		}
		
		boolean external = statement instanceof External;
		
		if (!statements.empty()) 
			external |= (Boolean) statements.peek()[5];
		
		Object[] item = {statement, name, md, new LinkedList<Node>(), the, external, null};
		statements.push(item);
		
	}
	
	public void endElement(String ns, String name) {

		try {
			
			Object[] currentItem = statements.pop();
			Statement currentStatement = (Statement) currentItem[0];
			
			if (currentStatement instanceof THE){
				Node node = (Node) currentItem[4];
				addChildren(node, (List<Node>) currentItem[3]);
				return;
			}
			
			boolean isProperty = currentStatement instanceof Property;
			boolean isRelation = currentStatement instanceof Relation;
			boolean isCachable = !((isProperty || isRelation) && !statements.empty());
			
			if (!isCachable){
				
				Object[] parentItem = statements.peek();
				Statement parentStatement = (Statement) parentItem[0];
				Node tmp = (Node) parentItem[6];
				boolean isTHE = parentStatement instanceof THE;
				
				if (isTHE) {
					tmp = (Node) parentItem[4]; 
				} else {
					if (tmp == null) {
						tmp = AnimoGraph.createNode();
						parentItem[6] = tmp;
					}
				}
				
				Operator operator = (Operator) currentStatement;
				Node res = operator.build(tmp, (String) currentItem[1]);
				
				if (isProperty)
					addChildren(res, (List<Node>) currentItem[3]);
				
				if (isTHE)
					return;
				
			} 
			
			MessageDigest md = (MessageDigest) currentItem[2];
			byte [] digest = md.digest();
			String hash = MessageDigester.byteArrayToHex(digest);
			
			THE the = THE.getInstance();
			
			if (isCachable){
				
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
			System.out.println("Error building element " + name);
			e.printStackTrace(System.out);
			tx.finish();
		}
		
	}

	public void attribute(String ns, String name, String value) {
		
		MLInstruction instruction = ATTRIBUTE.getInstance(); 
		
		Node node = parentNode();
		Node target = valueNode(value);
		instruction.build(node, target, ns, name);
		
		MessageDigest md = (MessageDigest) statements.peek()[2]; 
		md.update(instruction.namespace().getBytes());
		md.update(instruction.name().getBytes());
		md.update(ns.getBytes());
		md.update(name.getBytes());
		
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
			characters(TEXT.getInstance(), text);
		}
			
	}
		
	public void comment(String text) {
		characters(COMMENT.getInstance(), text);
	}
	
	public void cdata (String text) {
		characters(CDATA.getInstance(), text);
	}
	
	private void characters (MLInstruction instruction, String value){

		Node node = parentNode();
		Node target = valueNode(value);
		instruction.build(node, target);
		
		MessageDigest md = (MessageDigest) statements.peek()[2]; 
		md.update(instruction.namespace().getBytes());
		md.update(instruction.name().getBytes());
	
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
	
	private Node valueNode(String value) {
		
		MessageDigest md = md();
		md.update(value.getBytes());
		byte[] digest = md.digest();
		String hash = MessageDigester.byteArrayToHex(digest);
		
		THE the = THE.getInstance();
		Node cache = the.node(AnimoGraph.CACHE, hash);
		
		if (cache == null){
			cache = the.create(AnimoGraph.CACHE, hash);
			Properties.VALUE.set(cache, value);
		}
		
		if (!statements.empty()) {
			((MessageDigest) statements.peek()[2]).update(digest);
		}
		
		return cache;
	}

	private Node parentNode(){
		Object[] item = statements.peek();
		if (item[0] instanceof THE) {
			return (Node) item[4];
		} else {
			Node tmp = (Node) item[6];
			if (tmp == null){
				tmp = AnimoGraph.createNode();
				item[6] = tmp;
			}
			return tmp;
		}
	}
	
}
