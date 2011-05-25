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

import org.animotron.Namespaces;
import org.animotron.Statement;
import org.animotron.Statements;
import org.animotron.operator.Relation;
import org.animotron.operator.THE;
import org.animotron.operator.relation.HAVE;
import org.animotron.operator.relation.USE;
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
	
	private Transaction tx = AnimoGraph.beginTx();
	
	private int level = 0;
	
	private Stack<MessageDigest> mds = new Stack<MessageDigest>();
	private Stack<List<Node>> children = new Stack<List<Node>>();
	
	Stack<Statement> statements = new Stack<Statement>();
	
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
	
	public void startElement(String ns, String name) {
		
		level++;
		
		Statement parent = statements.peek();
		Statement statement = Statements.namespace(ns);
		
		statements.push(statement);
		
		if (statement instanceof Relation) 
			return;
		
		children.push(new LinkedList<Node>());
		
		if (statement instanceof THE)
			return;
			
		if (statement instanceof HAVE && parent instanceof THE)
			return;
			
		MessageDigest md = md();
		md.update(ns.getBytes());
		md.update(name.getBytes());
		mds.push(md);
		
	}

	public void endElement(String ns, String name) {
		
		level--;
		
		Statement statement = statements.pop();
		Statement parent = statements.peek();
		
		try {
			if (statement instanceof THE){
				THE the = (THE) statement;
				Node node = the.build(AnimoGraph.THE, name);
				addChildren(node, children.pop());
				if (level == 0) {
					setTHE(the.relationship(name));
				}
			} else if (parent instanceof THE && statement instanceof Relation){
				THE the = (THE) parent;
				Relation relation = (Relation) statement;
				relation.build(the.node(name), name);
				statements.pop();
			} else if (statement instanceof USE){
				THE the = (THE) statements.pop();
				USE use = (USE) statement;
				use.build(the.node(name));
			} else if (level == _level_ && Namespaces.HAVE.equals(ns)){
				List<Node> children = children.pop();
				createHAVE(_node_, name, children);
			} else {
				MessageDigest md = mds.pop();
				byte [] cache = md.digest();
				List<Node> children = children.pop();
				Node currentNode = getOrCreateCACHE(MessageDigester.byteArrayToHex(cache), ns, name, children);
				if (level > 0) {
					//add this node as child
					children.peek().add(currentNode);
					//update parent's
					if (level != _level_) {
						mds.peek().update(cache);
					}
				} else {
					setTHE(AnimoGraph.getRelationCACHE(name));
				}
			}

			if (level == 0) {
				tx.success();
				tx.finish();
			}
			
		} catch (Exception e){
			tx.finish();
		}
		
	}

	public void attribute(String ns, String name, String value) {
		return;
//		try {
//			MessageDigest md = CACHEStack.peek();
//			//CACHE-function depend on namespace, name & value
//			md.update(ns.getBytes());
//			md.update(name.getBytes());
//			md.update(value.getBytes());
//		} catch (Exception e){
//			tx.finish();
//		}
	}

	public void characters(String text) {
		return;
//		try {
//			MessageDigest md = CACHEStack.peek();
//			//CACHE-function depend on characters
//			md.update(text.getBytes());
//		} catch (Exception e){
//			tx.finish();
//		}
	}

	private Node getOrCreateCACHE(String cache, String ns, String name, List<Node> children) {
		Node node = AnimoGraph.getCACHE(cache);
		if (node == null){
			node = AnimoGraph.createCACHE(cache);
			addChildren(AnimoGraph.createElement(node, name, ns), children);
		}
		return node;
	}
	
	private Node createHAVE(Node the, String name, List<Node> children) {
		Node node = AnimoGraph.createHAVE(the, name);
		addChildren(node, children);
		return node;
	}
	
	private void addChildren(Node node, List<Node> children) {
		for (Node n : children) {
			for (Relationship r : n.getRelationships(Direction.OUTGOING)){
				node.createRelationshipTo(r.getEndNode(), r.getType());
			}
		}
	}
	
}
