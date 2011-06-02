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

import org.animotron.Properties;
import org.animotron.Statement;
import org.animotron.operator.Cachable;
import org.animotron.operator.Evaluable;
import org.animotron.operator.External;
import org.animotron.operator.THE;
import org.exist.security.MessageDigester;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.Transaction;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * 
 */
public abstract class AbstractGraphBuilder implements GraphHandler {
	
	private Relationship the = null;
	
	private static final String CACHE_ALGOTHIM = "SHA-256";
	
	private Stack<Object[]> statements;
	private List<Object[]> flow;

	private Transaction tx;
		
	final public Relationship getTHE() {
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
	
	@Override
	final public void startDocument(){
		statements = new Stack<Object[]>();
		flow = new LinkedList<Object[]>();
		tx = AnimoGraph.beginTx();
	};
	
	@Override
	final public void endDocument(){
		try {
			build();
			tx.success();
		} finally {
			tx.finish();
		}
	}

	@Override
	final public void start(Statement statement, String ns, String name, String value) {
		
		MessageDigest md = md();
		
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
		
		Node current = null;
		
		Object[] item = {	
				statement,	// 0 	
				ns, 		// 1
				name, 		// 2
				val, 		// 3
				md, 		// 4
				external, 	// 5
				current,	// 6
				parent, 	// 7
				false		// 8
			};
		
		statements.push(item);
		flow.add(item);
		
	}
	
	final protected void end(){
		Object[] current = statements.pop();
		byte[] hash = ((MessageDigest) current[4]).digest();
		if (!statements.empty()) {
			((MessageDigest) statements.peek()[4]).update(hash);
		}
		current[4] = hash;
	}
	
	private void build() {
		for (Object[] item : flow) {
			build(item);
		}
	}
	
	private void build(Object[] item){
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
					String h = Properties.HASH.get(node);
					if (h == null) {
						Properties.HASH.set(node, hash);
					} else if (!h.equals(hash)) {
						AnimoGraph.clear(node);
						Properties.HASH.set(node, hash);
					} else {
						item[8] = true;
					}
				} else {
					node = the.create(name);
					Properties.HASH.set(node, hash);
				}
			} else {
				Node parent = (Node) p[6];
				if (parent == null) {
					//TODO Fire exception
					return;
				}
				if (statement instanceof Cachable) {
					String hash = hash(item);
					node = AnimoGraph.getCache(hash);
					if (node == null) {
						node = build(statement, parent, item, p);
						AnimoGraph.createCache(node, hash);
					} else {
						parent.createRelationshipTo(node, statement.relationshipType());
						item[8] = true;
					}
				} else {
					node = build(statement, parent, item, p); 
				}
			}
			item[6] = node;
		} catch (Exception e){
			tx.finish();
		}
	}
	
	private String hash (byte[] md) {
		return MessageDigester.byteArrayToHex(md); 
	}
	
	private String hash(Object[] item) {
		return hash((byte[]) item[4]);
	}
	
	final public Node value(String value, byte[] bytes) {
		try{
			MessageDigest md = md();
			md.update(bytes);
			String hash = hash(md.digest());
			Node cache = AnimoGraph.getCache(hash);
			if (cache == null) {
				cache = AnimoGraph.createCache(hash);
				Properties.VALUE.set(cache, value);
			}
			return cache;
		} catch (Exception e){
			tx.finish();
			return null;
		}
	}
	
	private Node build(Statement statement, Node parent, Object[] item, Object[] p){
		Node node = statement.build(parent, (String) item[1], (String) item[2], (Node) item[3]);
		if (statement instanceof Evaluable && !(Boolean) p[5]) {
			AnimoGraph.CACHE.createRelationshipTo(node, RelationshipTypes.CALCULATE);
		}
		return node;
	}

	@Override
	final public void end(Statement statement, String ns, String name, String value) {
		end();
	}
	
}
