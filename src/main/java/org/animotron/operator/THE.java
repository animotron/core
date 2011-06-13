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
package org.animotron.operator;

import static org.animotron.Properties.HASH;
import static org.animotron.Properties.NAME;
import static org.animotron.graph.AnimoGraph.beginTx;
import static org.animotron.graph.AnimoGraph.clear;
import static org.animotron.graph.AnimoGraph.createNode;
import static org.animotron.graph.AnimoGraph.finishTx;
import static org.animotron.graph.AnimoGraph.getNode;
import static org.animotron.graph.AnimoGraph.getOrCreateNode;
import static org.animotron.graph.AnimoGraph.getROOT;
import static org.animotron.graph.AnimoGraph.getTOP;
import static org.neo4j.graphdb.Direction.OUTGOING;

import java.io.IOException;

import org.animotron.graph.AnimoRelationshipType;
import org.animotron.graph.RelationshipTypes;
import org.animotron.interpreter.Calculator;
import org.animotron.operator.relation.IS;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.RelationshipType;
import org.neo4j.graphdb.Transaction;

/**
 * Operator 'THE'.
 * 
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 */
public class THE extends AbstarctOperator {
	
	public static String NAMESPACE = "animo/instance";
	public static String PREFIX = "the";

	public static final THE _ = new THE();
	
	protected final Node THE_NODE;

	private THE() { 
		super(PREFIX, NAMESPACE); 
		Transaction tx = beginTx();
		try {
			THE_NODE = getOrCreateNode(getROOT(), RelationshipTypes.THE);
			tx.success();
		} finally {
			finishTx(tx);
		}
	}
	
	public Node NODE() {
		return THE_NODE;
	}

	public RelationshipType relashionshipType(String name){
		return AnimoRelationshipType.get(name(), name);
	}
	
	public Relationship relationship(String name){
		return relationship(THE_NODE, name);
	}
	
	public Relationship relationship(Node parent, String name) {
		RelationshipType type = relashionshipType(name);
		return parent.getSingleRelationship(type, OUTGOING);
	}
	
	public Node node(String name) {
		RelationshipType type = relashionshipType(name);
		return getNode(THE_NODE, type);
	}
	
	public Relationship create(String name, String hash) {
		Relationship r = create(name);
		HASH.set(r, hash);
		return r;
	}
	
	public Relationship create(String name) {
		Node node = createNode();
		RelationshipType type = relashionshipType(name);
		Relationship r = THE_NODE.createRelationshipTo(node, type);
		NAME.set(node, name);
		return r;
	}
	
	public Relationship getOrCreate(String name) {
		Relationship r = relationship(name);
		if (r == null) {
			r = create(name);
		}
		return r;
	}
	
	@Override
	public Relationship build(Node parent, String prefix, String ns, String name, Node value, int order) {
		Relationship r = relationship(name);
		if (r != null) {
			clear(r);
		} else {
			r = create(name);
		}
		return r;
	}
	
	@Override
	public String name(Relationship r) {
		return NAME.get(r.getEndNode());
	}
	
	public void prepare(Relationship op) {
		
		Transaction tx = beginTx();
		try {
			Node the = op.getEndNode(); 
			if (!the.hasRelationship(IS._.relationshipType(), OUTGOING)) {
				getTOP().createRelationshipTo(the, RelationshipTypes.TOP);
			}
			Calculator.push(op);
			tx.success();
		} finally {
			finishTx(tx);
		}
		
		try {
			Calculator.prepare(op);
		} catch (IOException e) {
			e.printStackTrace();
		}
		
	}

}
