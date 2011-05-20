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

import org.animotron.Properties;
import org.animotron.exist.index.AnimoIndex;
import org.animotron.exist.index.RelationshipTypeTHE;
import org.animotron.exist.index.RelationshipTypes;
import org.neo4j.graphdb.Direction;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.PropertyContainer;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.RelationshipType;
import org.neo4j.graphdb.Transaction;
import org.neo4j.graphdb.traversal.Evaluators;
import org.neo4j.kernel.Traversal;
import org.xml.sax.helpers.AttributesImpl;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * 
 */
public class AnimoGraph {

	private static Node root = AnimoIndex.graphDb.getReferenceNode();
	
	protected static Transaction beginTx() {
		return AnimoIndex.graphDb.beginTx();
	}
	
	public static String getProperty(PropertyContainer container, Properties key) {
		return container.getProperty(key.name()).toString();
	};
	
	public static void setProperty(PropertyContainer container, Properties key, String value) {
		if (value != null && !value.equals("")){
			container.setProperty(key.name(), value);
		}
	};
	
	protected static void clear (Node node){
		for (Relationship r : node.getRelationships(Direction.OUTGOING)){
			Node end = r.getEndNode();
			r.delete();
			if (!end.hasRelationship(Direction.INCOMING)) {
				root.createRelationshipTo(end, RelationshipTypes.GC);
			}
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
		node.createRelationshipTo(getOrCreateTHE(name), RelationshipTypes.REF);
		return node;
	}
	
	public static Node getTHE(String name) {
		return getNode(new RelationshipTypeTHE(name));
	}

	protected static Node createTHE(String name) {
		Node node = createNode();
		setProperty(node, Properties.NAME, name);
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
			setProperty(node, Properties.SOURCE, source);
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
	
	private static Node createNamedNode (Node parent, String name, String namespace, RelationshipType type){
		Node node = createNode(parent, type);
		setProperty(node, Properties.NAMESPACE, namespace);
		setProperty(node, Properties.NAME, name);
		return node;
	}

	protected static Node createElement(Node parent, String name, String namespace) {
		return createNamedNode(parent, name, namespace, RelationshipTypes.ELEMENT);
	}
	
	protected static Node createAttribute(Node parent, String name, String namespace, String value) {
		Node node = createNamedNode(parent, name, namespace, RelationshipTypes.ATTRIBUTE);
		setProperty(node, Properties.VALUE, value);
		return node;
	}
	
	private static Node createCharacterData(Node parent, String text, RelationshipType type) {
		Node node = createNode(parent, type);
		setProperty(node, Properties.VALUE, text);
		return node;
	}
	
	protected static Node createText(Node parent, String text) {
		return createCharacterData(parent, text, RelationshipTypes.TEXT);
	}
	
	protected static Node createComment(Node parent, String text) {
		return createCharacterData(parent, text, RelationshipTypes.COMMENT);
	}
	
	protected static Node createCDATA(Node parent, String text) {
		return createCharacterData(parent, text, RelationshipTypes.CDATA);
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
	
	private static Iterable<Node> resolveUpIsLogic(Node node) {
		return Traversal.description().
			breadthFirst().
			relationships(RelationshipTypes.IS).
			evaluator(Evaluators.excludeStartPosition()).breadthFirst().
			traverse(node).
			nodes();
	}

	private static Iterable<Node> resolveDownIsLogic(Node node) {
		return Traversal.description().
			breadthFirst().
			relationships(RelationshipTypes.IS).
			evaluator(Evaluators.excludeStartPosition()).breadthFirst().
			traverse(node).
			nodes();
	}	
	
	public static Relationship getTHErelation(String name) {
		return root.getSingleRelationship(new RelationshipTypeTHE(name), Direction.OUTGOING);
	}
	
}
