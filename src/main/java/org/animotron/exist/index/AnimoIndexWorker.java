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

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Stack;

import org.animotron.Namespaces;
import org.exist.collections.Collection;
import org.exist.dom.DocumentImpl;
import org.exist.dom.DocumentSet;
import org.exist.dom.ElementImpl;
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
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.Transaction;
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
		
		Transaction tx = AnimoIndex.graphDb.beginTx();
		try {
			Relationship r = 
				AnimoIndex.graphDb.getReferenceNode().getSingleRelationship(
						RelationshipTypes.THEs, Direction.OUTGOING );
			
			if (r == null) {
				instanceFactoryNode = AnimoIndex.graphDb.createNode();
				AnimoIndex.graphDb.getReferenceNode().createRelationshipTo(
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
	
	private class AnimoStreamListener extends AbstractStreamListener {

		//here because of optimization reasons
    	private Node THENode;
    	private Node activeNode = null;
    	
    	private Stack<Node> nodes = new Stack<Node>();
    	private Stack<Node> activeNodes = new Stack<Node>();
    	
    	private Stack<List<Node>> childrens = new Stack<List<Node>>();

    	private List<Boolean> animoNodes = new ArrayList<Boolean>();
    	
    	private int level = 0;
    	
        @Override
        public void startElement(Txn transaction, ElementImpl element, NodePath path) {
            if (mode == STORE) {
            	
            	boolean animo = false;
            	
            	String nsURI = element.getQName().getNamespaceURI();
            	if (nsURI == null || nsURI.isEmpty());
            	
            	else if (Namespaces.THE.namespace().equals(nsURI)) {
            		THENode = AnimoGraph.addTHE(element);
            		activeNode = THENode;
                	
            		animo = true;
            	
            	} else if (Namespaces.IS.namespace().equals(nsURI)) {
            		AnimoGraph.addIsRelationship(THENode, AnimoGraph.getOrCreateNode(instanceFactoryNode, element.getLocalName()));

            	} else if (Namespaces.HAVE.namespace().equals(nsURI)) {
            		activeNode = AnimoGraph.createExistNode(element);
            		
            		animo = true;
            	}

    			activeNodes.push(activeNode);
    			
    			setAnimoMark(level, animo);

    			Node currentNode = activeNode;
        		if (!animo) {
        			currentNode = AnimoGraph.createExistNode(element);
        		}
    			nodes.push(currentNode);
        		
    			if (level != 0)
    				//add current element as child 
    				childrens.peek().add(currentNode);
        		
        		//create empty children list
        		childrens.push(new ArrayList<Node>());
            	
            } else {
            	System.out.println("mode = "+mode+" path = "+path);
            }
            super.startElement(transaction, element, path);
            level++;
        }
        
        public void endElement(Txn transaction, ElementImpl element, NodePath path) {
            level--;
            if (mode == STORE) {
            	
            	activeNode = activeNodes.pop();
            	Node currentNode = nodes.pop();
            	List<Node> childs = childrens.pop();
            	
            	if (animoNodes.get(level)) {
            		for (Node child : childs) {
            			//XXX: ignore "IS" nodes
                		AnimoGraph.addProcessingInstruction(currentNode, child);
            		}
            	}
            }
        	super.endElement(transaction, element, path);
        }

    	@Override
		public IndexWorker getWorker() {
			return AnimoIndexWorker.this;
		}
    	
    	private void setAnimoMark(int index, boolean flag) {
    		for (int i = 0; i <= level; i++) {
    			if (animoNodes.size() <= i) 
    				animoNodes.add(flag);
    			else if (flag) 
    				animoNodes.set(i, true);
    		}
    	}
    	
    }
	
	//TODO: cycled relations (up & down relations)
}
