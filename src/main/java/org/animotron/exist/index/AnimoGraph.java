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
package org.animotron.exist.index;

import org.exist.dom.ElementAtExist;
import org.exist.dom.NewArrayNodeSet;
import org.exist.dom.NodeProxy;
import org.exist.dom.NodeSet;
import org.neo4j.graphdb.Direction;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.RelationshipType;
import org.neo4j.graphdb.Transaction;
import org.neo4j.graphdb.traversal.Evaluators;
import org.neo4j.kernel.Traversal;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * 
 */
public class AnimoGraph {

	protected static Transaction beginTx() {
		return AnimoIndex.graphDb.beginTx();
	}
	
	protected static void clear (Node node){
		for (Relationship r : node.getRelationships(Direction.OUTGOING)){
			r.delete();
		}
	}
	
	private static Node getNode(RelationshipType type) {
		Relationship r = AnimoIndex.graphDb.getReferenceNode().getSingleRelationship(type, Direction.OUTGOING);
		return r == null ? null : r.getEndNode();
	};

	private static Node createNode(Node parent, RelationshipType type) {
		Node node = AnimoIndex.graphDb.createNode();
		parent.createRelationshipTo(node, type);
		return node;
	}
	
	private static Node createNode(Node parent, RelationshipType type, String name) {
		Node node = createNode(parent, type);
		node.createRelationshipTo(getOrCreateTHE(name), RelationshipTypes.IS);
		return node;
	}
	
	protected static Node getTHE(String name) {
		return getNode(new RelationshipTypeTHE(name));
	}

	protected static Node createTHE(String name) {
		Node node = AnimoIndex.graphDb.createNode();
		AnimoIndex.graphDb.getReferenceNode().createRelationshipTo(node, new RelationshipTypeTHE(name));
		return node;
	}

	protected static Node getOrCreateTHE(String name) {
		Node node = getTHE(name);
		if (node == null){
			node = createTHE(name);
		}
		return node;
	}
	
	protected static Node createAN(Node parent, String name) {
		return createNode(parent, RelationshipTypes.AN, name);
	}

	protected static Node createANY(Node parent, String name) {
		return createNode(parent, RelationshipTypes.ANY, name);
	}

	protected static Node createALL(Node parent, String name) {
		return createNode(parent, RelationshipTypes.ALL, name);
	}

	protected static Node createHAVE(Node parent, String name) {
		return createNode(parent, RelationshipTypes.HAVE, name);
	}

	protected static Node createElement(Node parent, ElementAtExist element) {
		Node node = createNode(parent, RelationshipTypes.ELEMENT);
		node.setProperty("namespace", element.getNamespaceURI());
		node.setProperty("name", element.getLocalName());
		node.setProperty("prefix", element.getPrefix());
		return node;
	}
	
	private static void relationshipTo(Node start, Node end, RelationshipType type) {
		start.createRelationshipTo(end, type);
	}
	
	protected static void addIsRelationship(Node the, String is) {
		addIsRelationship(the, getOrCreateTHE(is));
	}
	
	protected static void addIsRelationship(Node the, Node is) {
		relationshipTo(the, is, RelationshipTypes.IS);
	}
	
	protected static void addUseRelationship(Node node, String is) {
		addUseRelationship(node, getOrCreateTHE(is));
	}
	
	protected static void addUseRelationship(Node node, Node is) {
		relationshipTo(node, is, RelationshipTypes.USE);
	}
	
	public static NodeSet resolveUpIsLogic(String name) {
		Node instance = AnimoGraph.getTHE(name);
		if (instance == null) return NodeSet.EMPTY_SET;
		NodeSet set = new NewArrayNodeSet(5);
		resolveUpIsLogic(instance);
		// TODO: Serialize to NodeSet
		return set;
	}

	public static NodeSet resolveUpIsLogic(NodeSet s) {
		NodeSet result = new NewArrayNodeSet(5);
		for (NodeProxy node : s) {
			Node instance = AnimoGraph.getTHE(node.getNode().getLocalName());
			if (instance == null) continue;
			resolveUpIsLogic(instance);
		}
		// TODO: Serialize to NodeSet
		return result;
	}

	private static Iterable<Node> resolveUpIsLogic(Node node) {
		return Traversal.description().
			breadthFirst().
			relationships(RelationshipTypes.IS ).
			evaluator(Evaluators.excludeStartPosition()).breadthFirst().
			traverse(node).
			nodes();
	}

	public static NodeSet resolveDownIsLogic(String name) {
		Node instance = AnimoGraph.getTHE(name);
		if (instance == null) return NodeSet.EMPTY_SET;
		NodeSet result = new NewArrayNodeSet(5);
		resolveDownIsLogic(instance);
		// TODO: Serialize to NodeSet
		return result;
	}

	public static NodeSet resolveDownIsLogic(NodeSet set) {
		NodeSet result = new NewArrayNodeSet(5);
		for (NodeProxy node : set) {
			Node instance = AnimoGraph.getTHE(node.getNode().getLocalName());
			if (instance == null) continue;
			resolveDownIsLogic(instance);
		}
		// TODO: Serialize to NodeSet
		return result;
	}

	private static Iterable<Node> resolveDownIsLogic(Node node) {
		return Traversal.description().
			breadthFirst().
			relationships(RelationshipTypes.IS ).
			evaluator(Evaluators.excludeStartPosition()).breadthFirst().
			traverse(node).
			nodes();
	}	
	
	public static NodeSet evaluate(String string) {
		return null;
	}	
}
