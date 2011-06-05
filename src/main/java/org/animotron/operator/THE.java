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

import org.animotron.Properties;
import org.animotron.graph.AnimoGraph;
import org.animotron.graph.AnimoRelationshipType;
import org.animotron.graph.RelationshipTypes;
import org.neo4j.graphdb.Direction;
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

	private static final THE INSTANCE = new THE();
	public static THE getInstance() { return INSTANCE; }
	
	protected final Node THE_NODE;

	private THE() { 
		super(PREFIX, NAMESPACE); 
		Transaction tx = AnimoGraph.beginTx();
		try {
			THE_NODE = AnimoGraph.getOrCreateNode(AnimoGraph.getROOT(), RelationshipTypes.THE);
			tx.success();
		} finally {
			tx.finish();
		}
	}

	public RelationshipType relashionshipType(String name){
		return AnimoRelationshipType.get(name(), name);
	}
	
	public Relationship relationship(String name){
		return relationship(THE_NODE, name);
	}
	
	public Relationship relationship(Node parent, String name) {
		RelationshipType type = relashionshipType(name);
		return parent.getSingleRelationship(type, Direction.OUTGOING);
	}
	
	public Node node(String name) {
		RelationshipType type = relashionshipType(name);
		return AnimoGraph.getNode(THE_NODE, type);
	}
	
	public Node create(String name) {
		Node node = AnimoGraph.createNode();
		RelationshipType type = relashionshipType(name);
		THE_NODE.createRelationshipTo(node, type);
		Properties.NAME.set(node, name);
		return node;
	}
	
	public Node getOrCreate(String name) {
		Node node = node(name);
		if (node == null) {
			node = create(name);
		}
		return node;
	}
	
	@Override
	public Node build(Node parent, String prefix, String ns, String name, Node value, int order) {
		Node node = node(name);
		if (node != null) {
			AnimoGraph.clear(node);
		} else {
			node = create(name);
		}
		return node;
	}
	
	@Override
	public String name(Relationship r) {
		return Properties.NAME.get(r.getEndNode());
	}


}
