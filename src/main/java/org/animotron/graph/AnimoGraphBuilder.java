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
import org.animotron.operator.Operator;
import org.animotron.operator.Property;
import org.animotron.operator.Reference;
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
	
	Stack<Object[]> statements;
	Object[] item = {null, null, null, null, null};
	Object[] childItem;
		
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
		childItem = item;
		tx = AnimoGraph.beginTx();
	};
	
	public void startElement(String ns, String name) {
		
		Statement statement = Statements.namespace(ns);
		
		if (statement == null) {
			statement = ELEMENT.getInstance(); 
		}

		Node the = null;
		//move the instance to GC & create new
		if (statement instanceof THE){
			the = ((THE) statement).build(AnimoGraph.THE, name);
		}
		
		MessageDigest md = md();
		md.update(ns.getBytes());
		md.update(name.getBytes());
		
		Object[] item = {statement, name, md, new LinkedList<Node>(), the};
		statements.push(item);
		
	}

	public void endElement(String ns, String name) {

		try {
			
			Object[] currentItem = statements.pop();
			Statement currentOperator = (Statement) currentItem[0];
			
			if (currentOperator instanceof THE){
				Node the = (Node) currentItem[4];
				addChildren(the, (List<Node>) currentItem[3]);
				childItem = currentItem; 
				return;
			}
			
			if (currentOperator instanceof Property && !statements.empty()){
				Object[] parentItem = statements.peek(); 
				Statement parentOperator = (Statement) parentItem[0]; 
				if (parentOperator instanceof THE || parentOperator instanceof Reference || parentOperator instanceof Property) {
					Operator operator = (Operator) currentOperator;
					Node the = (Node) parentItem[4];
					addChildren(operator.build(the, (String) childItem[1]), (List<Node>) currentItem[3]);
					return;
				}
			}
			
			Statement childOperator = (Statement) childItem[0]; 
			
			if ((currentOperator instanceof Reference || currentOperator instanceof THE) && childOperator instanceof Relation){
				Operator operator = (Operator) childOperator;
				Node the = (Node) currentItem[4];
				operator.build(the, (String) childItem[1]);
				
				if (currentOperator instanceof THE)
					return;
				
			}
			
			MessageDigest md = (MessageDigest) currentItem[2];
			byte [] digest = md.digest();
			String hash = MessageDigester.byteArrayToHex(digest);
			
			THE the = THE.getInstance();
			
			if (!(currentOperator instanceof Relation || currentOperator instanceof Property)){
				
				Node cache = the.node(AnimoGraph.CACHE, hash);
				
				if (cache == null){
					
					cache = the.create(AnimoGraph.CACHE, hash);
					
					if (currentOperator instanceof Operator) {
						Operator operator = (Operator) currentOperator;
						addChildren(operator.build(cache, name), (List<Node>) currentItem[3]);
						
					} else {
						ELEMENT element = ELEMENT.getInstance();
						addChildren(element.build(cache, ns, name), (List<Node>) currentItem[3]);
						
					}
					
					currentItem[4] = cache;
			
					if (!statements.empty())
						((List<Node>) statements.peek()[3]).add(cache);
					
				}
				
			}
			
			if (!statements.empty()) {
				((MessageDigest) statements.peek()[2]).update(digest);
			}
			
			childItem = currentItem;
			
		} catch (Exception e){
			e.printStackTrace(System.out);
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
