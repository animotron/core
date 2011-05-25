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

import org.animotron.Container;
import org.animotron.Statement;
import org.animotron.Statements;
import org.animotron.instruction.Instruction;
import org.animotron.instruction.ml.ELEMENT;
import org.animotron.operator.Operator;
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
	
	private int level = 0;
	
	private Stack<MessageDigest> mds = new Stack<MessageDigest>();
	private Stack<List<Node>> children = new Stack<List<Node>>();
	
	Stack<Instruction> instractions = new Stack<Instruction>();
	
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
		
		Statement statement = Statements.namespace(ns);
		
		Instruction instruction;
		
		if (statement instanceof Operator) {
			instruction = ((Operator) statement).instruction(name); 
		} else {
			instruction = ELEMENT.getInstance(); 
		}
		
		instractions.push(instruction);
		
		if (statement instanceof Relation) 
			return;
		
		children.push(new LinkedList<Node>());
		
		if (statement instanceof THE)
			return;
			
		Statement parent = instractions.peek();
		if (statement instanceof HAVE && parent instanceof THE)
			return;
			
		MessageDigest md = md();
		md.update(ns.getBytes());
		md.update(name.getBytes());
		mds.push(md);
		
	}

	public void endElement(String ns, String name) {
		
		level--;
		
		Instruction currentInstruction = instractions.pop();
		Container currentOperator = currentInstruction.container();
		Instruction parentInstruction = instractions.peek();
		Container parentOperator = parentInstruction.container();
		
		try {
			
			if (currentOperator instanceof THE){
				THE.Instruction the = (THE.Instruction) currentInstruction; 
				Node node = the.build(AnimoGraph.THE);
				addChildren(node, children.pop());
				if (level == 0) {
					//setTHE(the.relationship(name));
				}
				
			} else if (parentOperator instanceof THE && currentOperator instanceof Relation){
				Relation.Instruction relation = (Relation.Instruction) currentInstruction; 
				THE.Instruction the = (THE.Instruction) parentInstruction;
				Node node = the.getOrCreate(AnimoGraph.THE);
				relation.build(node);
				
			} else if (parentOperator instanceof THE && currentOperator instanceof HAVE){
				Reference.Instruction have = (Reference.Instruction) currentInstruction; 
				THE.Instruction the = (THE.Instruction) parentInstruction;
				Node node = have.build(the.getOrCreate(AnimoGraph.THE));
				addChildren(node, children.pop());
				
			} else {
				
				MessageDigest md = mds.pop();
				byte [] hash = md.digest();
				
				THE.Instruction the = THE.getInstance().instruction(MessageDigester.byteArrayToHex(hash)); 
				Node cache = the.get(AnimoGraph.CACHE);
				
				if (cache == null){
					
					cache = the.create(AnimoGraph.CACHE);
					
					Node node;
					
					if (currentOperator instanceof USE) {
						Relation.Instruction use = (Relation.Instruction) currentInstruction; 
						node = use.build(cache);
						
					} else if (currentOperator instanceof Reference) {
						Reference.Instruction reference = (Reference.Instruction) currentInstruction; 
						node = reference.build(cache);
						
					} else {
						ELEMENT element = ELEMENT.getInstance();
						node = element.build(cache, ns, name);
					}
						
					addChildren(node, children.pop());
				}

				if (level > 0) {
					children.peek().add(cache);
					if (!(parentOperator instanceof THE)) {
						mds.peek().update(hash);
					}
				} else {
					//setTHE(AnimoGraph.getRelationCACHE(name));
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

	private void addChildren(Node node, List<Node> children) {
		for (Node n : children) {
			for (Relationship r : n.getRelationships(Direction.OUTGOING)){
				node.createRelationshipTo(r.getEndNode(), r.getType());
			}
		}
	}
	
}
