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

import org.animotron.exist.index.AnimoIndex;
import org.neo4j.graphdb.Direction;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.RelationshipType;
import org.neo4j.graphdb.Transaction;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * 
 */
public class AnimoGraph {

	public final static Node ROOT = AnimoIndex.graphDb.getReferenceNode();
	public final static Node THE, CACHE, CALC, EMPTY, GC;
	
	static {
		Transaction tx = AnimoGraph.beginTx();
		try {
			GC = AnimoGraph.getOrCreateNode(ROOT, RelationshipTypes.GC);
			THE = AnimoGraph.getOrCreateNode(ROOT, RelationshipTypes.THE);
			CALC = AnimoGraph.getOrCreateNode(ROOT,RelationshipTypes.CALC);
			EMPTY = AnimoGraph.getOrCreateNode(ROOT,RelationshipTypes.EMPTY);
			CACHE = AnimoGraph.getOrCreateNode(ROOT, RelationshipTypes.CACHE);
			tx.success();
		} finally {
			tx.finish();
		}
	}
	
	protected static Transaction beginTx() {
		return AnimoIndex.graphDb.beginTx();
	}
	
	public static void clear (Node node){
		for (Relationship r : node.getRelationships(Direction.OUTGOING)){
			Node end = r.getEndNode();
			r.delete();
			if (!end.hasRelationship(Direction.INCOMING)) {
				GC.createRelationshipTo(end, RelationshipTypes.GARBAGE);
			}
		}
	}
	
	public static Node getNode(Node parent, RelationshipType type) {
		Relationship r = parent.getSingleRelationship(type, Direction.OUTGOING);
		return r == null ? null : r.getEndNode();
	};
	
	public static Node createNode(){
		return AnimoIndex.graphDb.createNode();
	}

	public static Node createNode(Node parent, RelationshipType type) {
		Node node = createNode();
		parent.createRelationshipTo(node, type);
		return node;
	}
	
	public static Node getOrCreateNode(Node parent, RelationshipType type) {
		Relationship r = parent.getSingleRelationship(type, Direction.OUTGOING);
		if (r != null)
			return r.getEndNode();
		Node node = createNode(parent, type);
		return node;
	}

}
