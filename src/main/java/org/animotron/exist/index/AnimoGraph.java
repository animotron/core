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
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.RelationshipType;
import org.neo4j.graphdb.Transaction;
import org.neo4j.graphdb.traversal.Evaluators;
import org.neo4j.kernel.Traversal;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * 
 */
public class AnimoGraph {

	protected static Transaction beginTx(){
		return AnimoIndex.graphDb.beginTx();
	}
	
	private static Node getNode(RelationshipType type, String name){
		return AnimoIndex.indexService.getSingleNode(type.name(), name);
	};

	private static Node createNode() {
		return AnimoIndex.graphDb.createNode();
	}
	private static Node createNode(String name) {
		Node node = createNode();
		node.setProperty("name", name);
		return node;
	}
	
	private static Node createNode(RelationshipType type, String name) {
		Node node = createNode(name);
        AnimoIndex.indexService.index(node, type.name(), name);
		AnimoIndex.graphDb.getReferenceNode().createRelationshipTo(node, type);
		return node;
	}
	
	private static Node getOrCreateNode(RelationshipType type, ElementAtExist element) {
		String name = element.getLocalName();
		Node node = getNode(type, name);
		if (node == null){
			node = createNode(type, name);
		}
		return node;
	}
	
	protected static synchronized Node getTHE(String name) {
		return getNode(RelationshipTypes.THE, name);
	}

	protected static Node getOrCreateTHE(ElementAtExist element) {
		return getOrCreateNode(RelationshipTypes.THE, element);
	}
	
	protected static void addIsRelationship(Node the, Node is) {
		the.createRelationshipTo(is, RelationshipTypes.IS);
	}
	
	protected static void addUseRelationship(Node the, Node is) {
		the.createRelationshipTo(is, RelationshipTypes.USE);
	}
	
	protected static Node createExistNode(Node parent, ElementAtExist element) {
		String name = element.getLocalName();
		Node node = createNode(name);
        parent.createRelationshipTo(node, RelationshipTypes.ELEMENT);
        return node;
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
