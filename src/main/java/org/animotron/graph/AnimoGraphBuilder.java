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
import org.animotron.instruction.ml.ValueInstruction;
import org.animotron.operator.CACHE;
import org.animotron.operator.Cachable;
import org.animotron.operator.External;
import org.animotron.operator.Operator;
import org.animotron.operator.THE;
import org.exist.security.MessageDigester;
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
	private static final CACHE CACHE_FACTORY = CACHE.getInstance();
	
	private Stack<Object[]> statements;
	private List<Object[]> flow;
		
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
		flow = new LinkedList<Object[]>();
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
	
	public void endElement(String ns, String name) {
		end();
	}
	
	public void endDocument(){
		Transaction tx = AnimoGraph.beginTx();
		try {
			build();
			tx.success();
		} finally {
			tx.finish();
		}
	}

	private void start(Statement statement, String ns, String name, String value) {
		
		MessageDigest md = md();
		
		if (ns != null) 
			md.update(ns.getBytes());
		
		if (name != null)
			md.update(name.getBytes());
		
		if (!statement.namespace().equals(ns)) 
			md.update(statement.namespace().getBytes());

		if (!statement.name().equals(name)) 
			md.update(statement.name().getBytes());

		byte[] bytes = null; 
		if (value != null) {
			bytes = value.getBytes();
			md.update(bytes);
		}
		
		boolean external = statement instanceof External;
		if (!statements.empty()) {
			external |= (Boolean) statements.peek()[6];
		}
		
		Node current = null;
		boolean passed = false; 
		List<Object[]> children = new LinkedList<Object[]>();
		
		Object[] item = {	
				statement,	// 0 	
				ns, 		// 1
				name, 		// 2
				value, 		// 3
				bytes, 		// 4
				md, 		// 5
				external, 	// 6
				current,	// 7
				children, 	// 8
				passed		// 9
			};
		
		statements.push(item);
		
	}
	
	private void end(){
		Object[] current = statements.pop();
		if (!statements.empty()) {
			Object[] parent = statements.peek();
			((MessageDigest) parent[5]).update(((MessageDigest) current[5]).digest());
			((List<Object>) parent[8]).add(current);
		} else {
			flow.add(current);
		}
	}
	
	private void build() {
		for (Object[] item : flow) {
			build(null, item);
		}
	}
	
	private void build(Node parent, List<Object[]> item) {
		for (Object[] child : item){
			build(parent, child);
		}
	}
	
	private void build(Node parent, Object[] item){
		if ((Boolean) item[9])
			return;
		Node node;
		Statement statement = (Statement) item[0];
		if (statement instanceof THE) {
			THE the = (THE) statement;
			String name = (String) item[2];
			node = the.build((String) name, hash(item));
		} else {
			if (parent == null) {
				//TODO Fire exception
				return;
			}
			if (statement instanceof Cachable) {
				String hash = hash(item);
				node = CACHE_FACTORY.node(hash);
				if (node == null) {
					if (statement instanceof Operator) {
						Operator operator = (Operator) statement;
						String name = (String) item[2];
						node = operator.build(parent, name); 
					} else if (statement instanceof ELEMENT) {
						ELEMENT element = (ELEMENT) statement;
						String ns = (String) item[1];
						String name = (String) item[2];
						node = element.build(parent, ns, name); 
					} else {
						Instruction instruction = (Instruction) statement;
						node = instruction.build(parent);
					}
					CACHE_FACTORY.build(node, hash);
				} else {
					parent.createRelationshipTo(node, statement.relationshipType());
					item[7] = node;
					item[9] = true;
					return;
				}
			} else {
				if (statement instanceof Operator) {
					Operator operator = (Operator) statement;
					String name = (String) item[2];
					node = operator.build(parent, name); 
				} else if (statement instanceof ATTRIBUTE) {
					ATTRIBUTE attribute = (ATTRIBUTE) statement;
					String ns = (String) item[1];
					String name = (String) item[2];
					Node value = value(item);
					node = attribute.build(parent, value, ns, name);
				} else if (statement instanceof ValueInstruction) {
					ValueInstruction instruction = (ValueInstruction) statement;
					Node value = value(item);
					node = instruction.build(parent, value);
				} else {
					Instruction instruction = (Instruction) statement;
					node = instruction.build(parent);
				}
			}
		}
		build(node, (List<Object[]>) item[8]);
		item[7] = node;
		item[9] = true;
	}
	
	private String hash (MessageDigest md) {
		return MessageDigester.byteArrayToHex(md.digest()); 
	}
	
	private String hash(Object[] item) {
		return hash((MessageDigest) item[5]);
	}
	
	private Node value(Object[] item) {
		if (item[4] == null)
			return null;
		byte[] bytes = (byte[]) item[4];
		MessageDigest md = md();
		md.update(bytes);
		String hash = hash(md);
		Node cache = CACHE_FACTORY.node(hash);
		if (cache == null) {
			cache = CACHE_FACTORY.build(hash);
			Properties.VALUE.set(cache, (String) item[3]);
		}
		return cache;
	}

}
