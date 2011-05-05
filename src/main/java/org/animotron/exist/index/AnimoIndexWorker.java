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

import java.util.Map;

import org.animotron.Namespaces;
import org.exist.collections.Collection;
import org.exist.dom.DocumentImpl;
import org.exist.dom.DocumentSet;
import org.exist.dom.ElementImpl;
import org.exist.dom.NewArrayNodeSet;
import org.exist.dom.NodeProxy;
import org.exist.dom.NodeSet;
import org.exist.dom.StoredNode;
import org.exist.indexing.AbstractStreamListener;
import org.exist.indexing.IndexController;
import org.exist.indexing.IndexWorker;
import org.exist.indexing.MatchListener;
import org.exist.indexing.OrderedValuesIndex;
import org.exist.indexing.QNamedKeysIndex;
import org.exist.indexing.StreamListener;
import org.exist.storage.DBBroker;
import org.exist.storage.NodePath;
import org.exist.storage.txn.Txn;
import org.exist.util.DatabaseConfigurationException;
import org.exist.util.Occurrences;
import org.exist.xquery.XQueryContext;
import org.neo4j.graphdb.Direction;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Path;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.Transaction;
import org.neo4j.graphdb.traversal.Evaluators;
import org.neo4j.graphdb.traversal.TraversalDescription;
import org.neo4j.kernel.Traversal;
import org.w3c.dom.NodeList;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class AnimoIndexWorker implements OrderedValuesIndex, QNamedKeysIndex {

	private int mode = 0;
    private DocumentImpl document = null;
    private AnimoIndex index;
    
    private final Node instanceFactoryNode;

	public AnimoIndexWorker(AnimoIndex index) {
		this.index = index;
		
		Transaction tx = index.graphDb.beginTx();
		try {
			Relationship r = 
				index.graphDb.getReferenceNode().getSingleRelationship(
						RelationshipTypes.THEs, Direction.OUTGOING );
			
			if (r == null) {
				instanceFactoryNode = index.graphDb.createNode();
				index.graphDb.getReferenceNode().createRelationshipTo(
						instanceFactoryNode, RelationshipTypes.THEs);
			} else {
				instanceFactoryNode = r.getEndNode();
			}
			tx.success();
		} finally {
			tx.finish();
		}
	}
	
	/* (non-Javadoc)
	 * @see org.exist.indexing.IndexWorker#getIndexId()
	 */
	public String getIndexId() {
		return AnimoIndex.ID;
	}

	/* (non-Javadoc)
	 * @see org.exist.indexing.IndexWorker#getIndexName()
	 */
	public String getIndexName() {
		return index.getIndexName();
	}

	/* (non-Javadoc)
	 * @see org.exist.indexing.IndexWorker#configure(org.exist.indexing.IndexController, org.w3c.dom.NodeList, java.util.Map)
	 */
	public Object configure(IndexController controller, NodeList configNodes,
			Map<String, String> namespaces)
			throws DatabaseConfigurationException {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see org.exist.indexing.IndexWorker#setDocument(org.exist.dom.DocumentImpl)
	 */
	public void setDocument(DocumentImpl doc) {
		this.document = doc;
	}

	/* (non-Javadoc)
	 * @see org.exist.indexing.IndexWorker#setDocument(org.exist.dom.DocumentImpl, int)
	 */
	public void setDocument(DocumentImpl doc, int mode) {
		this.document = doc;
		this.mode = mode;
		
		System.out.println("setDocument doc = "+doc+" mode = "+mode);
	}

	/* (non-Javadoc)
	 * @see org.exist.indexing.IndexWorker#setMode(int)
	 */
	public void setMode(int mode) {
		this.mode = mode;
		System.out.println("setMode mode = "+mode);
	}

	/* (non-Javadoc)
	 * @see org.exist.indexing.IndexWorker#getDocument()
	 */
	public DocumentImpl getDocument() {
		return document;
	}

	/* (non-Javadoc)
	 * @see org.exist.indexing.IndexWorker#getMode()
	 */
	public int getMode() {
		return mode;
	}

	/* (non-Javadoc)
	 * @see org.exist.indexing.IndexWorker#getReindexRoot(org.exist.dom.StoredNode, org.exist.storage.NodePath, boolean)
	 */
	public StoredNode getReindexRoot(StoredNode node, NodePath path, boolean includeSelf) {
		System.out.println("getReindexRoot path = "+path);
		// TODO Auto-generated method stub
		return null;
	}
	
	private StreamListener listener = new AnimoStreamListener();

	/* (non-Javadoc)
	 * @see org.exist.indexing.IndexWorker#getListener()
	 */
	public StreamListener getListener() {
		return listener;
	}

	/* (non-Javadoc)
	 * @see org.exist.indexing.IndexWorker#getMatchListener(org.exist.storage.DBBroker, org.exist.dom.NodeProxy)
	 */
	public MatchListener getMatchListener(DBBroker broker, NodeProxy proxy) {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see org.exist.indexing.IndexWorker#flush()
	 */
	public void flush() {
		
		// TODO Auto-generated method stub

	}

	/* (non-Javadoc)
	 * @see org.exist.indexing.IndexWorker#removeCollection(org.exist.collections.Collection, org.exist.storage.DBBroker)
	 */
	public void removeCollection(Collection collection, DBBroker broker) {
		System.out.println("removeCollection "+collection);
		// TODO Auto-generated method stub

	}

	/* (non-Javadoc)
	 * @see org.exist.indexing.IndexWorker#checkIndex(org.exist.storage.DBBroker)
	 */
	public boolean checkIndex(DBBroker broker) {
		System.out.println("checkIndex called");
		// TODO Auto-generated method stub
		return false;
	}

	/* (non-Javadoc)
	 * @see org.exist.indexing.IndexWorker#scanIndex(org.exist.xquery.XQueryContext, org.exist.dom.DocumentSet, org.exist.dom.NodeSet, java.util.Map)
	 */
	public Occurrences[] scanIndex(XQueryContext context, DocumentSet docs, NodeSet contextSet, Map hints) {
		// TODO Auto-generated method stub
		return null;
	}
	
	private synchronized THE addTHE(ElementImpl element) {
		final String name = element.getLocalName();
		
		THE instance = getTHE(name);
		
		if (instance != null) {
			if (instance.proxy.getDocument() == index.unresolvedReferenceDocument) {
				instance.update(element.getDocumentAtExist(), element.getNodeId());
			}
			//XXX: check doc with element's doc
			return instance;
		}
		Node node = null;
		
		Transaction tx = index.graphDb.beginTx();
		try {
	        node = index.graphDb.createNode();
	
	        node.setProperty("name", name);
	        index.indexService.index( node, "name", name );
	        
	        instanceFactoryNode.createRelationshipTo( node, RelationshipTypes.THE );
	        
	        tx.success();
		} finally {
			tx.finish();
		}
        
        return new THE( node, element );
	}
	
	private synchronized THE getTHE(String name) {
		Node node = index.indexService.getSingleNode( "name", name );
		if (node != null) {
			return new THE(index, node);
		}
		return null;
	}
	
	
	public synchronized THE getOrCreateNode(String name) {
		THE instance = getTHE(name);
		if (instance != null) return instance;
		
		Transaction tx = index.graphDb.beginTx();
		try {
	        Node node = index.graphDb.createNode();
	
	        node.setProperty("name", name);
	        index.indexService.index( node, "name", name );
	        
	        instanceFactoryNode.createRelationshipTo( node, RelationshipTypes.THE );
	        
	        tx.success();

			return new THE(node, 
					index.unresolvedReferenceDocument, 
					index.unresolvedReferenceId.newChild());
		} finally {
			tx.finish();
		}
	}

	public NodeProxy getNode(String name) {
		THE instance = getTHE(name);
		if (instance == null) return null;
		
		return instance.proxy;
	}

	private class AnimoStreamListener extends AbstractStreamListener {

    	private THE currentNode;
    	
        @Override
        public void startElement(Txn transaction, ElementImpl element, NodePath path) {
            if (mode == STORE) {
            	if (Namespaces.THE.namespace().equals(element.getQName().getNamespaceURI())) {
            		currentNode = addTHE(element);
            	
            	} else if (Namespaces.IS.namespace().equals(element.getQName().getNamespaceURI())) {
            		currentNode.addIsRelationship(getOrCreateNode(element.getLocalName()));
            	}
            } else {
            	System.out.println("mode = "+mode+" path = "+path);
            }
            super.startElement(transaction, element, path);
        }

    	@Override
		public IndexWorker getWorker() {
			return AnimoIndexWorker.this;
		}
    	
    }

	public NodeSet resolveUpIsLogic(String name) {

		THE instance = getTHE(name);
		if (instance == null) return NodeSet.EMPTY_SET;
		
		NodeSet set = new NewArrayNodeSet(5);
		
		resolveUpIsLogic(instance, set);
		
		return set;
	}

	public NodeSet resolveUpIsLogic(NodeSet s) {
		NodeSet result = new NewArrayNodeSet(5);
		
		THE instance = null;
		for (NodeProxy node : s) {
		
			instance = getTHE(node.getNode().getLocalName());
			if (instance == null) continue;
			
			resolveUpIsLogic(instance, result);
		}
		//TODO: to check: is empty?
		return result;
	}

	private void resolveUpIsLogic(THE node, NodeSet result) {
		TraversalDescription td = Traversal.description().
			breadthFirst().
			relationships(RelationshipTypes.IS ).
			evaluator(Evaluators.excludeStartPosition());
		
		for ( Path path : td.traverse( node.graphNode ) ) {
			result.add(  (new THE(index, path.endNode())).proxy );
		}
	}

	public NodeSet resolveDownIsLogic(String name) {
		
		THE instance = getTHE(name);
		if (instance == null) return NodeSet.EMPTY_SET;
		
		NodeSet result = new NewArrayNodeSet(5);
		
		resolveDownIsLogic(instance, result);
		
		return result;
	}

	public NodeSet resolveDownIsLogic(NodeSet set) {
		NodeSet result = new NewArrayNodeSet(5);
		
		THE instance = null;
		for (NodeProxy node : set) {
			
			instance = getTHE(node.getNode().getLocalName());
			if (instance == null) continue;
			
			resolveDownIsLogic(instance, set);
		}
		//TODO: to check: is empty?
		return result;
	}

	private void resolveDownIsLogic(THE node, NodeSet result) {
		TraversalDescription td = Traversal.description().
			breadthFirst().
			relationships(RelationshipTypes.IS ).
			evaluator(Evaluators.excludeStartPosition());
		
		for ( Path path : td.traverse( node.graphNode ) ) {
			result.add(  (new THE(index, path.endNode())).proxy );
		}
	}
	
	//TODO: cycled relations (up & down relations)
}
