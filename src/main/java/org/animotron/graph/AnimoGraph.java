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
	public final static Node THE, CACHE, CALC;
	
	static {
		Transaction tx = AnimoGraph.beginTx();
		try {
			THE = AnimoGraph.getOrCreateNode(ROOT, RelationshipTypes.THE);
			CACHE = AnimoGraph.getOrCreateNode(ROOT, RelationshipTypes.CACHE);
			CALC = AnimoGraph.getOrCreateNode(ROOT,RelationshipTypes.CALC);
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
				ROOT.createRelationshipTo(end, RelationshipTypes.GC);
			}
		}
	}
	
	public static Node getNode(Node parent, RelationshipType type) {
		Relationship r = parent.getSingleRelationship(type, Direction.OUTGOING);
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
	
	private static Node getOrCreateNode(Node parent, RelationshipType type) {
		Relationship r = parent.getSingleRelationship(type, Direction.OUTGOING);
		if (r != null)
			return r.getEndNode();
		Node node = createNode();
		parent.createRelationshipTo(node, type);
		return node;
	}
	
	private static Node createNode(Node parent, RelationshipType type, String name) {
		Node node = createNode(parent, type);
		node.createRelationshipTo(getOrCreateTHE(name), RelationshipTypes.REF);
		return node;
	}
	
	protected static Node getOrCreateTHE(String name) {
		Node node = getTHE(name);
		if (node == null){
			node = createTHE(name);
		}
		return node;
	}
	
	public static Node getCACHE(String name) {
		return getNode(CACHE, new AnimoRelationshipType(name));
	}

	protected static Node createCACHE(String name) {
		return createNode(CACHE, new AnimoRelationshipType(name));
	}

	protected static Node getOrCreateCACHE(String name) {
		Node node = getCACHE(name);
		if (node == null){
			node = createCACHE(name);
		}
		return node;
	}
	
	private static Node createNode(Node parent, RelationshipType type, String name, String source) {
		Node node = createNode(parent, type, name);
		if (source != null){
			Properties.SOURCE.set(node, source);
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
		Properties.NAMESPACE.set(node, namespace);
		Properties.NAME.set(node, name);
		return node;
	}

	protected static Node createElement(Node parent, String name, String namespace) {
		return createNamedNode(parent, name, namespace, RelationshipTypes.ELEMENT);
	}
	
	protected static Node createAttribute(Node parent, String name, String namespace, String value) {
		Node node = createNamedNode(parent, name, namespace, RelationshipTypes.ATTRIBUTE);
		Properties.VALUE.set(node, value);
		return node;
	}
	
	private static Node createCharacterData(Node parent, String text, RelationshipType type) {
		Node node = createNode(parent, type);
		Properties.VALUE.set(node, text);
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
	
	public static Relationship getRelationTHE(String name) {
		return ROOT.getSingleRelationship(new AnimoRelationshipType(name), Direction.OUTGOING);
	}
	
	public static Relationship getRelationCACHE(String name) {
		return CACHE.getSingleRelationship(new AnimoRelationshipType(name), Direction.OUTGOING);
	}

}
