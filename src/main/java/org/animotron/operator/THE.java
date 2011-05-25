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

import org.animotron.graph.AnimoGraph;
import org.animotron.graph.AnimoRelationshipType;
import org.animotron.instruction.AbstractInstruction;
import org.animotron.instruction.Instruction;
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
	
	public Node build(Node parent, String name) {
		RelationshipType type = relashionshipType(name);
		Node node = AnimoGraph.getNode(parent, type);
		if (node != null) {
			AnimoGraph.clear(node);
		}
		node = AnimoGraph.createNode(parent, type);
		return node;
	}
	
	public Relationship relationship(String name){
		return AnimoGraph.THE.getSingleRelationship(relashionshipType(name), Direction.OUTGOING);
	}
	
	public Node node(String name){
		return relationship(name).getEndNode();
	}

	@Override
	public Instruction instruction(String name) {
		return new THE.Instruction(this, name);
	}
	
	private class Instruction extends AbstractInstruction {
		
		public Instruction(Operator container, String name) {
			super(container, name);
		}
		
		@Override
		public Node build(Node parent){
			Node node = AnimoGraph.createNode(parent, relationshipType());
			node.createRelationshipTo(THE.getInstance().node(name()), AnimoRelationshipType.get("REF"));
			return node;
		}
		
	}
	
}
