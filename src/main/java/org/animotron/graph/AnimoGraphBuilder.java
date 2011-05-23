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
	
	private static final String HASH_ALGOTHIM = "SHA-256";
	
	private Transaction tx = AnimoGraph.beginTx();
	
	private int level = 0;
	
	private Stack<MessageDigest> hashStack = new Stack<MessageDigest>();
	private Stack<List<Node>> childrenStack = new Stack<List<Node>>();
	
	private Node nodeTHE = null; int levelTHE = 0;

	Stack<Node> nodeStackTHE = new Stack<Node>();
	Stack<Integer> levelStackTHE = new Stack<Integer>();
	
	public void startElement(String ns, String name) {
		
		level++;
		
		try {
			if (Namespaces.THE.equals(ns)){
				if (level > 1) {
					levelStackTHE.push(levelTHE);
					nodeStackTHE.push(nodeTHE);
				}
				levelTHE = level;
				nodeTHE = AnimoGraph.getOrCreateTHE(name);
				childrenStack.push(new LinkedList<Node>());
				
			} else if (Namespaces.IS.equals(ns)) {
				return;
				
			} else {
				MessageDigest md;
				try {
					md = MessageDigest.getInstance(HASH_ALGOTHIM);
				} catch (NoSuchAlgorithmException e) {
					//can't be, but throw runtime error
					throw new RuntimeException(e);
				}
				//hash-function depend on namespace & name
				md.update(ns.getBytes());
				md.update(name.getBytes());
				hashStack.push(md);
				childrenStack.push(new LinkedList<Node>());
			}
		} catch (Exception e){
			tx.finish();
		}
		
	}

	public void endElement(String ns, String name) {
		
		level--;
		
		try {
			if (Namespaces.THE.equals(ns)){
				addChildren(nodeTHE, childrenStack.pop());
				if (level > 0) {
					nodeTHE = nodeStackTHE.pop();
					levelTHE = levelStackTHE.pop();
				}
			} else if (level == levelTHE && Namespaces.IS.equals(ns)){
				AnimoGraph.addIsRelationship(nodeTHE, AnimoGraph.getOrCreateTHE(name));
			} else if (level == levelTHE && Namespaces.USE.equals(ns)){
				AnimoGraph.addUseRelationship(nodeTHE, AnimoGraph.getOrCreateTHE(name));
			} else if (level == levelTHE && Namespaces.HAVE.equals(ns)){
				List<Node> children = childrenStack.pop();
				createHAVE(nodeTHE, name, children);
			} else {
				MessageDigest md = hashStack.pop();
				byte [] hash = md.digest();
				List<Node> children = childrenStack.pop();
				Node currentNode = getOrCreateHASH(MessageDigester.byteArrayToHex(hash), ns, name, children);
				if (level > 0) {
					//add this node as child
					childrenStack.peek().add(currentNode);
					//update parent's
					if (level != levelTHE) {
						hashStack.peek().update(hash);
					}
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
//			MessageDigest md = hashStack.peek();
//			//hash-function depend on namespace, name & value
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
//			MessageDigest md = hashStack.peek();
//			//hash-function depend on characters
//			md.update(text.getBytes());
//		} catch (Exception e){
//			tx.finish();
//		}
	}

	private Node getOrCreateHASH(String hash, String ns, String name, List<Node> children) {
		Node node = AnimoGraph.getHASH(hash);
		if (node == null){
			node = AnimoGraph.createHASH(hash);
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
		for (Node n : children){
			for (Relationship r : n.getRelationships(Direction.OUTGOING))
			node.createRelationshipTo(r.getEndNode(), r.getType());
		}
	}
	
}
