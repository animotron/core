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

import org.animotron.Statement;
import org.animotron.Statements;
import org.animotron.instruction.ml.ELEMENT;
import org.animotron.operator.Reference;
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
	
	Stack<Object[]> statements = new Stack<Object[]>();
	
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
		
		Statement statement = Statements.namespace(ns);
		
		if (statement == null) {
			statement = ELEMENT.getInstance(); 
		}
		
		MessageDigest md = md();
		md.update(ns.getBytes());
		md.update(name.getBytes());
		
		Object[] item = {statement, name, md, new LinkedList<Node>()};
		
		statements.push(item);
		
	}

	public void endElement(String ns, String name) {
		

		try {
			
			Object[] currentItem = statements.pop();
			Statement currentOperator = (Statement) currentItem[0];
			
			if (currentOperator instanceof THE){
				THE the = (THE) currentOperator;
				Node node = the.build(AnimoGraph.THE, name);
				addChildren(node, (List<Node>) currentItem[3]);
				if (statements.empty()) {
					tx.success();
					tx.finish();
				}
				return;
			}
			
			if (!statements.empty()) {

				Object[] parentItem = statements.peek();
				Statement parentOperator = (Statement) parentItem[0];
				
				if (parentOperator instanceof THE && currentOperator instanceof Relation){
					Relation relation = (Relation) currentOperator;
					THE the = (THE) parentOperator;
					Node node = the.getOrCreate(AnimoGraph.THE, (String) parentItem[1]);
					relation.build(node, (String) currentItem[1]);
					return;
					
				} else if (parentOperator instanceof THE && currentOperator instanceof HAVE){
					Reference have = (Reference) currentOperator; 
					THE the = (THE) parentOperator;
					Node node = have.build(the.getOrCreate(AnimoGraph.THE, (String) parentItem[1]), name);
					addChildren(node, (List<Node>) currentItem[3]);
					return;
					
				}
			}
				
			MessageDigest md = (MessageDigest) currentItem[2];
			byte [] digest = md.digest();
			String hash = MessageDigester.byteArrayToHex(digest);
			
			THE the = THE.getInstance(); 
			Node cache = the.node(AnimoGraph.CACHE, hash);
			
			if (cache == null){
				
				cache = the.create(AnimoGraph.CACHE, hash);
				
				if (currentOperator instanceof USE) {
					Relation use = (Relation) currentOperator; 
					use.build(cache, name);
					
				} else if (currentOperator instanceof Reference) {
					Reference reference = (Reference) currentOperator;
					addChildren(reference.build(cache, name), (List<Node>) currentItem[3]);
					
				} else {
					ELEMENT element = ELEMENT.getInstance();
					addChildren(element.build(cache, ns, name), (List<Node>) currentItem[3]);
					
				}
				
			}
			
			if (statements.empty()) {
				tx.success();
				tx.finish();
				
			} else {
				Object[] parentItem = statements.peek();
				((MessageDigest) parentItem[2]).update(digest);
				((List<Node>) parentItem[3]).add(cache);
				
			}
			
		} catch (Exception e){
			System.out.println(e);
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

	private void addChildren(Node node, List<Node> children) {
		for (Node n : children) {
			for (Relationship r : n.getRelationships(Direction.OUTGOING)){
				node.createRelationshipTo(r.getEndNode(), r.getType());
			}
		}
	}
	
}
