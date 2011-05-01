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
import org.neo4j.graphdb.GraphDatabaseService;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Transaction;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class THE {
	
    public static final String KEY_DOC_ID = "docId";
    public static final String KEY_DOC_URI = "docURI";
    public static final String KEY_NODE_ID = "nodeId";
	
    protected final Node graphNode;
	protected NodeProxy proxy = null;
	
	public THE(final AnimoIndex index, final Node gNode) {
		graphNode = gNode;
		
		final Database db = index.getBrokerPool();

		final GraphDatabaseService graphDb = graphNode.getGraphDatabase();
		final Transaction tx = graphDb.beginTx();
		try {
			byte[] temp = (byte[]) gNode.getProperty(KEY_NODE_ID);
	        int units = ByteConversion.byteToShort(temp, 0);
	        NodeId nodeId = db.getNodeFactory().createFromData(units, temp, 2);
	        
			XmldbURI docURI = XmldbURI.create((String)gNode.getProperty( KEY_DOC_URI ));
			
			DocumentImpl doc = index.unresolvedReferenceDocument;
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
			
			proxy = new NodeProxy(doc, nodeId);
			
			tx.success();
		} finally {
			tx.finish();
		}
	}
	
	public THE(final Node gNode, final ElementAtExist node) {
		this(gNode, node.getDocumentAtExist(), node.getNodeId());
	}
	

	public THE(final Node gNode, final DocumentAtExist doc, final NodeId nodeId) {
		graphNode = gNode;
		
		update(doc, nodeId);
	}
	
	protected void update(final DocumentAtExist doc, final NodeId nodeId) {
		final GraphDatabaseService graphDb = graphNode.getGraphDatabase();
		final Transaction tx = graphDb.beginTx();
		try {
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
			
			proxy = new NodeProxy((DocumentImpl) doc, nodeId);//XXX: ???
			
			tx.success();
		} finally {
			tx.finish();
		}
	}

	public void addIsRelationship(final THE is) {
		final GraphDatabaseService graphDb = graphNode.getGraphDatabase();
		final Transaction tx = graphDb.beginTx();
		try {
			graphNode.createRelationshipTo( is.graphNode, RelationshipTypes.IS );

			tx.success();
		} finally {
			tx.finish();
		}
	}
}
