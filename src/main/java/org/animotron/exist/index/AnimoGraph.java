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

import org.exist.dom.NewArrayNodeSet;
import org.exist.dom.NodeProxy;
import org.exist.dom.NodeSet;
import org.exist.xquery.value.Type;
import org.neo4j.graphdb.Direction;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.RelationshipType;
import org.neo4j.graphdb.Transaction;
import org.neo4j.graphdb.traversal.Evaluators;
import org.neo4j.kernel.Traversal;
import org.w3c.dom.Attr;
import org.w3c.dom.CharacterData;
import org.w3c.dom.Element;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * 
 */
public class AnimoGraph {

	private static Node root = AnimoIndex.graphDb.getReferenceNode();
	
	protected static Transaction beginTx() {
		return AnimoIndex.graphDb.beginTx();
	}
	
	protected static void clear (Node node){
		for (Relationship r : node.getRelationships(Direction.OUTGOING)){
			r.delete();
		}
	}
	
	private static Node getNode(RelationshipType type) {
		Relationship r = root.getSingleRelationship(type, Direction.OUTGOING);
		return r == null ? null : r.getEndNode();
	};
	
	private static Node createNode(){
		return AnimoIndex.graphDb.createNode();
	}

	private static Node createNode(Node parent, RelationshipType type) {
		Node node = createNode();
		parent.createRelationshipTo(node, type);
		return node;
	}
	
	private static Node createNode(Node parent, RelationshipType type, String name) {
		Node node = createNode(parent, type);
		node.createRelationshipTo(getOrCreateTHE(name), RelationshipTypes.IS);
		return node;
	}
	
	public static Node getTHE(String name) {
		return getNode(new RelationshipTypeTHE(name));
	}

	protected static Node createTHE(String name) {
		Node node = createNode();
		root.createRelationshipTo(node, new RelationshipTypeTHE(name));
		return node;
	}

	protected static Node getOrCreateTHE(String name) {
		Node node = getTHE(name);
		if (node == null){
			node = createTHE(name);
		}
		return node;
	}
	
	//TODO: make deferred linking for every input context;  
	private static Node createNode(Node parent, RelationshipType type, String name, String source) {
		Node node = createNode(parent, type, name);
		if (source != null){
			node.setProperty("source", source);
		}
		return node;
	}

	protected static Node createAN(Node parent, String name, String source) {
		return createNode(parent, RelationshipTypes.AN, name, source);
	}

	protected static Node createANY(Node parent, String name, String source) {
		return createNode(parent, RelationshipTypes.ANY, name, source);
	}

	protected static Node createALL(Node parent, String name, String source) {
		return createNode(parent, RelationshipTypes.ALL, name, source);
	}

	protected static Node createPTRN(Node parent, String name) {
		return createNode(parent, RelationshipTypes.PTRN, name);
	}
	
	protected static Node createHAVE(Node parent, String name) {
		return createNode(parent, RelationshipTypes.HAVE, name);
	}
	
	protected static Node createIC(Node parent, String name) {
		return createNode(parent, RelationshipTypes.IC, name);
	}
	
	protected static Node createGT(Node parent, String name) {
		return createNode(parent, RelationshipTypes.GT, name);
	}
	
	protected static Node createGE(Node parent, String name) {
		return createNode(parent, RelationshipTypes.GE, name);
	}
	
	protected static Node createLT(Node parent, String name) {
		return createNode(parent, RelationshipTypes.LT, name);
	}
	
	protected static Node createLE(Node parent, String name) {
		return createNode(parent, RelationshipTypes.LE, name);
	}
	
	protected static Node createEQ(Node parent, String name) {
		return createNode(parent, RelationshipTypes.EQ, name);
	}
	
	protected static Node createNE(Node parent, String name) {
		return createNode(parent, RelationshipTypes.NE, name);
	}
	
	protected static Node createGET(Node parent, String name) {
		return createNode(parent, RelationshipTypes.GET, name);
	}
	
	protected static Node createSELF(Node parent, String name) {
		return createNode(parent, RelationshipTypes.SELF, name);
	}
	
	protected static Node createXQUERY(Node parent) {
		return createNode(parent, RelationshipTypes.XQUERY);
	}
	
	protected static Node createXSLT(Node parent) {
		return createNode(parent, RelationshipTypes.XSLT);
	}
	
	private static Node createNamedNode (Node parent, org.w3c.dom.Node n, RelationshipType type){
		Node node = createNode(parent, type);
		node.setProperty("namespace", n.getNamespaceURI());
		node.setProperty("name", n.getLocalName());
		node.setProperty("prefix", n.getPrefix());
		return node;
	}

	protected static Node createElement(Node parent, Element element) {
		return createNamedNode(parent, element, RelationshipTypes.ELEMENT);
	}
	
	protected static Node createAttribute(Node parent, Attr attribute) {
		Node node = createNamedNode(parent, attribute, RelationshipTypes.ATTRIBUTE);
		node.setProperty("value", attribute.getNodeValue());
		return node;
	}
	
	private static Node createCharacterData(Node parent, CharacterData text, RelationshipType type) {
		Node node = createNode(parent, type);
		node.setProperty("value", text.getNodeValue());
		return node;
	}
	
	protected static Node createCharacterData(Node parent, CharacterData text) {
		Node node = null;
		if (text.getNodeType() == Type.TEXT) {
			node = createCharacterData(parent, text, RelationshipTypes.TEXT);
		} else if (text.getNodeType() == Type.COMMENT) {
			node = createCharacterData(parent, text, RelationshipTypes.COMMENT);
		} else if (text.getNodeType() == Type.CDATA_SECTION) {
			node = createCharacterData(parent, text, RelationshipTypes.CDATA);
		}
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
			relationships(RelationshipTypes.IS).
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
			relationships(RelationshipTypes.IS).
			evaluator(Evaluators.excludeStartPosition()).breadthFirst().
			traverse(node).
			nodes();
	}	
	
	public static NodeSet evaluate(String string) {
		return null;
	}	
}
