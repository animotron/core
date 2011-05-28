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
import org.neo4j.graphdb.Direction;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.RelationshipType;

/**
 * Operator 'THE'.
 * 
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 */
public class THE extends Operator {
	
	private static final THE INSTANCE = new THE();
	public static THE getInstance() { return INSTANCE; }
	
	private THE() { super("the", "animo/instance"); }
	
	public RelationshipType relashionshipType(String name){
		return AnimoRelationshipType.get(name(), name);
	}
	
	public Relationship relationship(String name){
		return relationship(AnimoGraph.THE, name);
	}
	
	public Relationship relationship(Node parent, String name) {
		RelationshipType type = relashionshipType(name);
		return parent.getSingleRelationship(type, Direction.OUTGOING);
	}
	
	public Node node(String name){
		return node(AnimoGraph.THE, name);
	}

	public Node node(Node parent, String name) {
		RelationshipType type = relashionshipType(name);
		Node node = AnimoGraph.getNode(parent, type);
		return node;
	}
	
	public Node create(Node parent, String name) {
		RelationshipType type = relashionshipType(name);
		Node node = AnimoGraph.createNode(parent, type);
		Properties.NAME.set(node, name);
		return node;
	}
	
	public Node getOrCreate(String name) {
		return getOrCreate(AnimoGraph.THE, name);
	}
	
	public Node getOrCreate(Node parent, String name) {
		Node node = node(parent, name);
		if (node == null) {
			node = create(parent, name);
		}
		return node;
	}
	
	@Override
	public Node build(Node parent, String name) {
		Node node = node(parent, name);
		if (node != null) {
			AnimoGraph.clear(node);
		} else {
			node = create(parent, name);
		}
		return node;
	}
	
	@Override
	public Node build(Node parent, Node child, String name) {
		// TODO: throw exception
		return null; 
	}
	

}
