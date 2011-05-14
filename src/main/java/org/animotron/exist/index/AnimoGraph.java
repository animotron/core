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

import net.sf.saxon.type.Type;

import org.exist.Database;
import org.exist.dom.DocumentAtExist;
import org.exist.dom.DocumentImpl;
import org.exist.dom.ElementAtExist;
import org.exist.dom.ElementImpl;
import org.exist.dom.NewArrayNodeSet;
import org.exist.dom.NodeProxy;
import org.exist.dom.NodeSet;
import org.exist.numbering.NodeId;
import org.exist.security.PermissionDeniedException;
import org.exist.security.Subject;
import org.exist.storage.DBBroker;
import org.exist.util.ByteConversion;
import org.exist.xmldb.XmldbURI;
import org.neo4j.graphdb.GraphDatabaseService;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Path;
import org.neo4j.graphdb.Transaction;
import org.neo4j.graphdb.traversal.Evaluators;
import org.neo4j.graphdb.traversal.TraversalDescription;
import org.neo4j.kernel.Traversal;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * 
 */
public class AnimoGraph {

	private static final String SEPARATOR = "-";

	private static final String KEY_DOC_ID = "docId";
	private static final String KEY_DOC_URI = "docURI";
	private static final String KEY_NODE_ID = "nodeId";
	private static final String KEY_NODE_TYPE = "nodeType";

	// Return ID of node
	protected static String getUniqNodeId(ElementAtExist element) {
		return String.valueOf(element.getDocumentAtExist().getDocId())
				+ SEPARATOR + String.valueOf(element.getNodeId().units());
	}
	
	protected static Node addTHE(ElementImpl element) {
		String name = element.getLocalName();
		Transaction tx = AnimoIndex.graphDb.beginTx();
		try {
			Node instance = getTHE(name);
			if (instance != null) {
				if (getNodeProxy(instance).getDocument() == AnimoIndex.unresolvedReferenceDocument) {
					update(instance, element);
				}
				//XXX: check doc with element's doc
				return instance;
			}
			Node node = AnimoIndex.graphDb.createNode();
	        node.setProperty("name", name);
	        AnimoIndex.indexService.index(node, "THE", name);
			update(node, element);
	        tx.success();
	        return node;
		} finally {
			tx.finish();
		}
	}
	
	public static Node getNode(ElementAtExist element) {
		String id = getUniqNodeId(element);
		Node node = AnimoIndex.indexService.getSingleNode( "eXistID", id );
		if (node != null) {
			return node;
		}
		Transaction tx = AnimoIndex.graphDb.beginTx();
		try {
	        node = AnimoIndex.graphDb.createNode();
	        AnimoIndex.indexService.index( node, "eXistID", id );
	        tx.success();
			return node;
		} finally {
			tx.finish();
		}
	}

	protected static Node getOrCreateNode(Node instanceFactoryNode, String name) {
		Node instance = getTHE(name);
		if (instance != null) return instance;
		Transaction tx = AnimoIndex.graphDb.beginTx();
		try {
	        Node node = AnimoIndex.graphDb.createNode();
	        node.setProperty("name", name);
	        AnimoIndex.indexService.index( node, "THE", name );
	        instanceFactoryNode.createRelationshipTo( node, RelationshipTypes.THE );
	        tx.success();
			update(node, 
					AnimoIndex.unresolvedReferenceDocument, 
					AnimoIndex.unresolvedReferenceId.newChild());
			return node;
		} finally {
			tx.finish();
		}
	}

	public static NodeProxy getNode(String name) {
		Node instance = getTHE(name);
		if (instance == null) return null;
		return getNodeProxy(instance);
	}
	
	protected static void addIsRelationship(Node the, Node is) {
		final GraphDatabaseService graphDb = the.getGraphDatabase();
		final Transaction tx = graphDb.beginTx();
		try {
			the.createRelationshipTo(is, RelationshipTypes.IS);
			tx.success();
		} finally {
			tx.finish();
		}
	}
	
	protected static synchronized Node getTHE(String name) {
		return AnimoIndex.indexService.getSingleNode("THE", name);
	}

	protected static void addProcessingInstruction(Node graphNode, Node node) {
		final GraphDatabaseService graphDb = graphNode.getGraphDatabase();
		final Transaction tx = graphDb.beginTx();
		try {
			graphNode.createRelationshipTo( node, RelationshipTypes.PROCESSING_FLOW_ELEMENT );
			tx.success();
		} finally {
			tx.finish();
		}
	}

	protected static NodeProxy getNodeProxy(Node node) {
		NodeProxy proxy = null;
		final Database db = AnimoIndex.getInstance().getBrokerPool();
		final GraphDatabaseService graphDb = node.getGraphDatabase();
		final Transaction tx = graphDb.beginTx();
		try {
			byte[] temp = (byte[]) node.getProperty(KEY_NODE_ID);
			int units = ByteConversion.byteToShort(temp, 0);
			NodeId nodeId = db.getNodeFactory().createFromData(units, temp, 2);
			XmldbURI docURI = XmldbURI.create((String) node.getProperty(KEY_DOC_URI));
			DocumentImpl doc = AnimoIndex.unresolvedReferenceDocument;
			if (!docURI.equals("")) {
				DBBroker broker = db.getActiveBroker();
				Subject currentSubject = broker.getSubject();
				try {
					broker.setSubject(db.getSecurityManager().getSystemSubject());
					doc = (DocumentImpl) broker.getXMLResource(docURI);
				} catch (PermissionDeniedException e) {
					// can't be
					e.printStackTrace();
				} finally {
					broker.setSubject(currentSubject);
				}
			}
			proxy = new NodeProxy(doc, nodeId, Type.ELEMENT);
			tx.success();
		} finally {
			tx.finish();
		}
		return proxy;
	}
	
	protected static Node createExistNode(ElementAtExist node) {
		Transaction tx = AnimoIndex.graphDb.beginTx();
		try {
			Node graphNode = AnimoIndex.graphDb.createNode();
			String name = node.getLocalName();
			graphNode.setProperty("name", name);
	        
			AnimoIndex.indexService.index( graphNode, "name", name );
	        AnimoIndex.indexService.index( graphNode, "eXistID", getUniqNodeId(node) );
	        
	        //parentAnimoNode.createRelationshipTo(graphNode, RelationshipTypes.PROCESSING_FLOW_ELEMENT );
	        
	        update(graphNode, node);
			
	        tx.success();
			
	        return graphNode;
		} finally {
			tx.finish();
		}
	}

	
	
	private static void update(Node node, ElementAtExist element) {
		update(node, element.getDocumentAtExist(), element.getNodeId());
	}
	
	private static void update(Node node, DocumentAtExist doc, NodeId nodeId) {
		if (doc == null) {
			node.setProperty(KEY_DOC_ID, 0);
			node.setProperty(KEY_DOC_URI, "");
		} else {
			node.setProperty(KEY_DOC_ID, doc.getDocId());
			node.setProperty(KEY_DOC_URI, doc.getURI().toString());
		}
		
        // store the node id
        int nodeIdLen = nodeId.size();
        byte[] data = new byte[nodeIdLen + 2];
        ByteConversion.shortToByte((short) nodeId.units(), data, 0);
        nodeId.serialize(data, 2);

        node.setProperty(KEY_NODE_ID, data);
	}

	public static NodeSet resolveUpIsLogic(String name) {
		Node instance = AnimoGraph.getTHE(name);
		if (instance == null) return NodeSet.EMPTY_SET;
		NodeSet set = new NewArrayNodeSet(5);
		resolveUpIsLogic(instance, set);
		return set;
	}

	public static NodeSet resolveUpIsLogic(NodeSet s) {
		NodeSet result = new NewArrayNodeSet(5);
		for (NodeProxy node : s) {
			Node instance = AnimoGraph.getTHE(node.getNode().getLocalName());
			if (instance == null) continue;
			resolveUpIsLogic(instance, result);
		}
		//TODO: to check: is empty?
		return result;
	}

	private static void resolveUpIsLogic(Node node, NodeSet result) {
		TraversalDescription td = Traversal.description().
			breadthFirst().
			relationships(RelationshipTypes.IS ).
			evaluator(Evaluators.excludeStartPosition());
		for (Path path : td.traverse(node)) {
			result.add(AnimoGraph.getNodeProxy(path.endNode()));
		}
	}

	public static NodeSet resolveDownIsLogic(String name) {
		Node instance = AnimoGraph.getTHE(name);
		if (instance == null) return NodeSet.EMPTY_SET;
		NodeSet result = new NewArrayNodeSet(5);
		resolveDownIsLogic(instance, result);
		return result;
	}

	public static NodeSet resolveDownIsLogic(NodeSet set) {
		NodeSet result = new NewArrayNodeSet(5);
		for (NodeProxy node : set) {
			Node instance = AnimoGraph.getTHE(node.getNode().getLocalName());
			if (instance == null) continue;
			resolveDownIsLogic(instance, set);
		}
		//TODO: to check: is empty?
		return result;
	}

	private static void resolveDownIsLogic(Node node, NodeSet result) {
		TraversalDescription td = Traversal.description().
			breadthFirst().
			relationships(RelationshipTypes.IS ).
			evaluator(Evaluators.excludeStartPosition());
		for ( Path path : td.traverse(node)) {
			result.add(AnimoGraph.getNodeProxy(path.endNode()));
		}
	}

	public static NodeSet evaluate(String string) {
		return null;
	}	
}
