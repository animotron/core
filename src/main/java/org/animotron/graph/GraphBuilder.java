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

import static org.animotron.Properties.HASH;
import static org.animotron.Properties.VALUE;
import static org.animotron.graph.AnimoGraph.beginTx;
import static org.animotron.graph.AnimoGraph.clear;
import static org.animotron.graph.AnimoGraph.createCache;
import static org.animotron.graph.AnimoGraph.getCache;
import static org.animotron.graph.AnimoGraph.order;

import java.security.MessageDigest;
import java.util.LinkedList;
import java.util.List;
import java.util.Stack;
import java.util.StringTokenizer;

import org.animotron.MessageDigester;
import org.animotron.Statement;
import org.animotron.Statements;
import org.animotron.instruction.ml.ELEMENT;
import org.animotron.interpreter.Calculator;
import org.animotron.operator.Cachable;
import org.animotron.operator.External;
import org.animotron.operator.THE;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.Transaction;

/**
 * Animo graph builder, it do optimization/compression and 
 * inform listeners on store/delete events.
 * 
 * Direct graph as input from top element to bottom processing strategy.
 * 
 * Methods to use:
 * 
 * startGraph()
 * endGraph()
 * 
 * start(String prefix, String ns, String name, String value)
 * start(Statement statement, String prefix, String ns, String name, String value)
 * end()
 * 
 * getRelationship()
 * 
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 */
public abstract class GraphBuilder {
	
	private Relationship the = null;
	
	private Stack<Object[]> statements;
	private List<Object[]> flow;

	private Transaction tx;

	public final Relationship getRelationship() {
		return the;
	}
	
	final protected void startGraph() {
		statements = new Stack<Object[]>();
		flow = new LinkedList<Object[]>();
		the = null;
		tx = beginTx();
	};
	
	final public boolean successful(){
		return the != null;
	}
	
	final protected void endGraph(){
		Object[] first = flow.get(0);
		try {
			int i = 0;
			if (!(first[0] instanceof THE)) {
				Object[] item = {	
						THE._,				// 0 	
						THE.NAMESPACE,		// 1
						first[2],			// 2
						null, 				// 3
						first[4], 			// 4
						false,				// 5
						null,				// 6
						null,	 			// 7
						false,				// 8
						THE.PREFIX,			// 9
						false				// 10
					};
				first[7] = item; 
				build(item, i++);
				first = item;
			}
			for (Object[] item : flow) {
				build(item, i++);
			}
			tx.success();
		} finally {
			AnimoGraph.finishTx(tx);
			the = THE._.relationship((String) first[2]);
		}
		
		if (!((Boolean) first[10])) {
			Calculator.onStore(the);
		}
	}

	final protected void start(String prefix, String ns, String name, String value) {
		
		Statement statement = Statements.namespace(ns, name);
		if (statement == null) {
			statement = ELEMENT._;
		}
		
		start(statement, prefix, ns, name, value);
		
	}
	
	final protected void start(Statement statement, String prefix, String ns, String name, String value) {
		
		MessageDigest md = MessageDigester.md();
		
		if (ns != null) 
			md.update(ns.getBytes());
		
		if (name != null)
			md.update(name.getBytes());
		
		if (!statement.namespace().equals(ns)) 
			md.update(statement.namespace().getBytes());

		if (!statement.name().equals(name)) 
			md.update(statement.name().getBytes());
		
		Node val = null;
		if (value != null) {
			byte[] bytes = value.getBytes();
			val = value(value, bytes);
			md.update(bytes);
		}
		
		Object[] parent = null;
		boolean external = statement instanceof External;
		if (!statements.empty()) {
			parent = statements.peek();
			external |= (Boolean) parent[5];
		}
		
		Object[] item = {	
				statement,	// 0  statement 	
				ns, 		// 1  namespace
				name, 		// 2  name
				val, 		// 3  balue
				md, 		// 4  message gigest
				external, 	// 5  is exteral sentense
				null,		// 6  current node
				parent, 	// 7  parent item
				false,		// 8  builded
				prefix,		// 9  prefix
				false		//10  cached
			};
		
		statements.push(item);
		flow.add(item);
		
	}
	
	final protected void end() {
		Object[] current = statements.pop();
		byte[] hash = ((MessageDigest) current[4]).digest();
		if (!statements.empty()) {
			((MessageDigest) statements.peek()[4]).update(hash);
		}
		current[4] = hash;
	}
	
	final protected String removeWS(String value) {
		
		StringBuilder buf = new StringBuilder();
		
		if (value.length() > 0) {
			StringTokenizer tok = new StringTokenizer(value);
			while (tok.hasMoreTokens()) {
                buf.append(tok.nextToken());
				if (tok.hasMoreTokens()) buf.append(' ');
			}
		}
		
		if (buf.length() > 0)
			return buf.toString();
			
		return null;
		
	}
	
	//TODO: Store hash for every node as byte[]
	//TODO: Build graph via single thread in sync and async modes 
	
	private void build(Object[] item, int order){
		Object[] p =  (Object[]) item[7];
		if (p != null) {
			if ((Boolean) p[8]) {
				item[8] = true; 
				return;
			}
		}
		try {
			Node node;
			Statement statement = (Statement) item[0];
			if (statement instanceof THE) {
				THE the = (THE) statement;
				String name = (String) item[2];
				String hash = hash(item);
				node = the.node(name);
				if (node != null) {
					if (HASH.has(node)) {
						String h = HASH.get(node);
						if (h == null) {
							HASH.set(node, hash);
						} else if (!h.equals(hash)) {
							clear(node);
							HASH.set(node, hash);
						} else {
							item[8] = item[10] = true;
						}
					} else {
						HASH.set(node, hash);
					}
				} else {
					node = the.create(name, hash);
				}
			} else {
				Node parent = (Node) p[6];
				if (parent == null) {
					//TODO Fire exception
					return;
				}
				if (statement instanceof Cachable) {
					String hash = hash(item);
					node = getCache(hash);
					if (node == null) {
						node = build(statement, parent, item, p, order);
						createCache(node, hash);
					} else {
						Relationship r = parent.createRelationshipTo(node, statement.relationshipType());
						order(r, order);
						item[8] = item [10] = true;
					}
				} else {
					node = build(statement, parent, item, p, order); 
				}
			}
			item[6] = node;
		} catch (Exception e){
			fail(e);
		}
	}
	
	private String hash (byte[] md) {
		return MessageDigester.byteArrayToHex(md); 
	}
	
	private String hash(Object[] item) {
		return hash((byte[]) item[4]);
	}
	
	private Node value(String value, byte[] bytes) {
		try{
			MessageDigest md = MessageDigester.md();
			md.update(bytes);
			String hash = hash(md.digest());
			Node cache = getCache(hash);
			if (cache == null) {
				cache = createCache(hash);
				VALUE.set(cache, value);
			}
			return cache;
		} catch (Exception e){
			fail(e);
			return null;
		}
	}
	
	private Node build(Statement statement, Node parent, Object[] item, Object[] p, int order){
		Node node = statement.build(parent, (String) item[9], (String) item[1], (String) item[2], (Node) item[3], order);
		return node;
	}
	
	protected void fail(Exception e){
		e.printStackTrace(System.out);
		tx.finish();
	}
	
}
