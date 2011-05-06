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
import org.exist.dom.NodeProxy;
import org.exist.numbering.NodeId;
import org.exist.security.PermissionDeniedException;
import org.exist.security.Subject;
import org.exist.storage.DBBroker;
import org.exist.util.ByteConversion;
import org.exist.xmldb.XmldbURI;
import org.neo4j.graphdb.Direction;
import org.neo4j.graphdb.GraphDatabaseService;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.RelationshipType;
import org.neo4j.graphdb.ReturnableEvaluator;
import org.neo4j.graphdb.StopEvaluator;
import org.neo4j.graphdb.Transaction;
import org.neo4j.graphdb.Traverser;
import org.neo4j.graphdb.Traverser.Order;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class AnimoNode implements Node {
	
    public static final String KEY_DOC_ID = "docId";
    
    public static final String KEY_DOC_URI = "docURI";
    public static final String KEY_NODE_ID = "nodeId";
    
    public static final String KEY_NODE_TYPE = "nodeType";
    
    //////////////////////////////////////////////////////
	
    protected final Node graphNode;
	protected NodeProxy proxy = null;
	
	public AnimoNode(final Node gNode) {
		graphNode = gNode;
		
		final Database db = AnimoIndex.getInstance().getBrokerPool();

		final GraphDatabaseService graphDb = graphNode.getGraphDatabase();
		final Transaction tx = graphDb.beginTx();
		try {
			byte[] temp = (byte[]) gNode.getProperty(KEY_NODE_ID);
	        int units = ByteConversion.byteToShort(temp, 0);
	        NodeId nodeId = db.getNodeFactory().createFromData(units, temp, 2);
	        
			XmldbURI docURI = XmldbURI.create((String)gNode.getProperty( KEY_DOC_URI ));
			
			DocumentImpl doc = AnimoIndex.unresolvedReferenceDocument;
			if (!docURI.equals("")) {
				DBBroker broker = db.getActiveBroker();
				Subject currentSubject = broker.getSubject();
				try {
					broker.setSubject(db.getSecurityManager().getSystemSubject());
					
					doc = (DocumentImpl) broker.getXMLResource(docURI);
		
				} catch (PermissionDeniedException e) {
					//can't be
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
	}
	
	public AnimoNode(final Node gNode, final ElementAtExist node) {
		this(gNode, node.getDocumentAtExist(), node.getNodeId());
	}
	
	public AnimoNode(final GraphDatabaseService graphDb, final ElementAtExist node, final Node parentAnimoNode) {

		final Transaction tx = graphDb.beginTx();
		try {
			graphNode = graphDb.createNode();
			
			String name = node.getLocalName();
			
			graphNode.setProperty("name", name);
	        AnimoIndex.indexService.index( graphNode, "name", name );
	        
	        AnimoIndex.indexService.index( graphNode, "eXistID", Utils.getUniqNodeId(node) );
	        
	        parentAnimoNode.createRelationshipTo( graphNode, RelationshipTypes.PROCESSING_FLOW_ELEMENT );

	        update(node.getDocumentAtExist(), node.getNodeId());

			tx.success();
		} finally {
			tx.finish();
		}
			
	}


	public AnimoNode(final Node gNode, final DocumentAtExist doc, final NodeId nodeId) {
		graphNode = gNode;
		
		final GraphDatabaseService graphDb = graphNode.getGraphDatabase();
		final Transaction tx = graphDb.beginTx();
		try {
			update(doc, nodeId);

			tx.success();
		} finally {
			tx.finish();
		}
	}
	
	protected void update(final DocumentAtExist doc, final NodeId nodeId) {
		if (doc == null) {
			graphNode.setProperty( KEY_DOC_ID, 0 );
			graphNode.setProperty( KEY_DOC_URI, "" );
		} else {
			graphNode.setProperty( KEY_DOC_ID, doc.getDocId() );
			graphNode.setProperty( KEY_DOC_URI, doc.getURI().toString() );
		}
		
        // store the node id
        int nodeIdLen = nodeId.size();
        byte[] data = new byte[nodeIdLen + 2];
        ByteConversion.shortToByte((short) nodeId.units(), data, 0);
        nodeId.serialize(data, 2);

        graphNode.setProperty( KEY_NODE_ID, data );
		
		proxy = new NodeProxy((DocumentImpl) doc, nodeId, Type.ELEMENT);//XXX: ???
	}

	//neo4j methods
	
	@Override
	public GraphDatabaseService getGraphDatabase() {
		return graphNode.getGraphDatabase();
	}

	@Override
	public boolean hasProperty(String key) {
		return graphNode.hasProperty(key);
	}

	@Override
	public Object getProperty(String key) {
		return graphNode.getProperty(key);
	}

	@Override
	public Object getProperty(String key, Object defaultValue) {
		return graphNode.getProperty(key, defaultValue);
	}

	@Override
	public void setProperty(String key, Object value) {
		graphNode.setProperty(key, value);
	}

	@Override
	public Object removeProperty(String key) {
		return graphNode.removeProperty(key);
	}

	@Override
	public Iterable<String> getPropertyKeys() {
		return graphNode.getPropertyKeys();
	}

	@Override
	public Iterable<Object> getPropertyValues() {
		return graphNode.getPropertyValues();
	}

	@Override
	public long getId() {
		return graphNode.getId();
	}

	@Override
	public void delete() {
		graphNode.delete();
	}

	@Override
	public Iterable<Relationship> getRelationships() {
		return graphNode.getRelationships();
	}

	@Override
	public boolean hasRelationship() {
		return graphNode.hasRelationship();
	}

	@Override
	public Iterable<Relationship> getRelationships(RelationshipType... types) {
		return graphNode.getRelationships(types);
	}

	@Override
	public boolean hasRelationship(RelationshipType... types) {
		return graphNode.hasRelationship(types);
	}

	@Override
	public Iterable<Relationship> getRelationships(Direction dir) {
		return graphNode.getRelationships(dir);
	}

	@Override
	public boolean hasRelationship(Direction dir) {
		return graphNode.hasRelationship(dir);
	}

	@Override
	public Iterable<Relationship> getRelationships(RelationshipType type, Direction dir) {
		return graphNode.getRelationships(type, dir);
	}

	@Override
	public boolean hasRelationship(RelationshipType type, Direction dir) {
		return graphNode.hasRelationship(type, dir);
	}

	@Override
	public Relationship getSingleRelationship(RelationshipType type, Direction dir) {
		return graphNode.getSingleRelationship(type, dir);
	}

	@Override
	public Relationship createRelationshipTo(Node otherNode, RelationshipType type) {
		return graphNode.createRelationshipTo(otherNode, type);
	}

	@Override
	public Traverser traverse(Order traversalOrder,
			StopEvaluator stopEvaluator,
			ReturnableEvaluator returnableEvaluator,
			RelationshipType relationshipType, Direction direction) {

		return graphNode.traverse(traversalOrder, stopEvaluator, returnableEvaluator, relationshipType, direction);
	}

	@Override
	public Traverser traverse(Order traversalOrder,
			StopEvaluator stopEvaluator,
			ReturnableEvaluator returnableEvaluator,
			RelationshipType firstRelationshipType, Direction firstDirection,
			RelationshipType secondRelationshipType, Direction secondDirection) {

		return graphNode.traverse(traversalOrder, stopEvaluator, returnableEvaluator, firstRelationshipType, firstDirection, secondRelationshipType, secondDirection);
	}

	@Override
	public Traverser traverse(Order traversalOrder,
			StopEvaluator stopEvaluator,
			ReturnableEvaluator returnableEvaluator,
			Object... relationshipTypesAndDirections) {

		return graphNode.traverse(traversalOrder, stopEvaluator, returnableEvaluator, relationshipTypesAndDirections);
	}

	public void addProcessingInstruction(Node node) {
		final GraphDatabaseService graphDb = graphNode.getGraphDatabase();
		final Transaction tx = graphDb.beginTx();
		try {
			graphNode.createRelationshipTo( node, RelationshipTypes.PROCESSING_FLOW_ELEMENT );

			tx.success();
		} finally {
			tx.finish();
		}
	}
}
