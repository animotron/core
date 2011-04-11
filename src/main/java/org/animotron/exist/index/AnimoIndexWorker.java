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

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

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
import org.w3c.dom.NodeList;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class AnimoIndexWorker implements OrderedValuesIndex, QNamedKeysIndex {

	private int mode = 0;
    private DocumentImpl document = null;
    private AnimoIndex index;

	public AnimoIndexWorker(AnimoIndex index) {
		this.index = index;
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
	}

	/* (non-Javadoc)
	 * @see org.exist.indexing.IndexWorker#setMode(int)
	 */
	public void setMode(int mode) {
		this.mode = mode;
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
	public StoredNode getReindexRoot(StoredNode node, NodePath path,
			boolean includeSelf) {
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
		// TODO Auto-generated method stub

	}

	/* (non-Javadoc)
	 * @see org.exist.indexing.IndexWorker#checkIndex(org.exist.storage.DBBroker)
	 */
	public boolean checkIndex(DBBroker broker) {
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
	
	//instance name -> reference
	private Map<String, NodeProxy> names = new HashMap<String, NodeProxy>();
	
	//TODO: make this map do the job
	private Map<NodeProxy, String> ids = new HashMap<NodeProxy, String>(); 
	
	private NodeProxy addNode(ElementImpl element) {
		String name = element.getLocalName();
		if (names.containsKey(name)) { 
			NodeProxy proxy = names.get(name);
			if (proxy.getDocument() == index.unresolvedReferenceDocument) {
				proxy.update(element);
			}
			return proxy;
		
		} else {
			NodeProxy proxy = new NodeProxy(element);
			names.put(name, proxy);
			return proxy;
		}
	}
	
	public NodeProxy getOrCreateNode(String name) {
		NodeProxy node = names.get(name);
		
		if (node == null) {
			node = new NodeProxy(
					index.unresolvedReferenceDocument, 
					index.unresolvedReferenceId.newChild());
			names.put(name, node);
		}
		return node;
	}

	public NodeProxy getNode(String name) {
		return names.get(name);
	}
	
	public String getNodeName(NodeProxy id) {
		return ids.get(id);
	}

	//instance -> is-relations nodes (down)
	private Map<NodeProxy, Set<NodeProxy>> downIsRelations = new HashMap<NodeProxy, Set<NodeProxy>>(); 
	
	//instance -> is-relations nodes (up)
	private Map<NodeProxy, Set<NodeProxy>> upIsRelations = new HashMap<NodeProxy, Set<NodeProxy>>(); 

	
	private void addIsRelationship(NodeProxy node, NodeProxy is) {
		
		//record down relations
		Set<NodeProxy> nodes = null;
		if (downIsRelations.containsKey(is)) {
			nodes = downIsRelations.get(is);
		} else {
			nodes = new HashSet<NodeProxy>();
			
			downIsRelations.put(is, nodes);
		}
		
		nodes.add(node);

		//record up relations
		nodes = null;
		if (upIsRelations.containsKey(node)) {
			nodes = upIsRelations.get(node);
		} else {
			nodes = new HashSet<NodeProxy>();
			
			upIsRelations.put(node, nodes);
		}
		
		nodes.add(is);
	}

	private class AnimoStreamListener extends AbstractStreamListener {

    	private NodeProxy currentNode;
    	
        @Override
        public void startElement(Txn transaction, ElementImpl element, NodePath path) {
            if (mode == STORE) {
            	if (Namespaces.THE.namespace().equals(element.getQName().getNamespaceURI())) {
            		currentNode = addNode(element);
            	
            	} else if (Namespaces.IS.namespace().equals(element.getQName().getNamespaceURI())) {
            		addIsRelationship(currentNode, getOrCreateNode(element.getLocalName()));
            	}
            }
            super.startElement(transaction, element, path);
        }

    	@Override
		public IndexWorker getWorker() {
			return AnimoIndexWorker.this;
		}
    	
    }

	public NodeSet resolveUpIsLogic(String name) {
		NodeProxy node = getNode(name);
		
		if (!upIsRelations.containsKey(node))
			return NodeSet.EMPTY_SET;
		
		NodeSet set = new NewArrayNodeSet(5);
		
		resolveUpIsLogic(node, set);
		
		return set;
	}

	private void resolveUpIsLogic(NodeProxy node, NodeSet set) {
		if (!upIsRelations.containsKey(node))
			return;
		
		for (NodeProxy n : upIsRelations.get(node)) {
			set.add(n);
			resolveUpIsLogic(n, set);
		}
	}

	public NodeSet resolveDownIsLogic(String name) {
		NodeProxy node = getNode(name);
		
		if (!downIsRelations.containsKey(node))
			return NodeSet.EMPTY_SET;
		
		NodeSet set = new NewArrayNodeSet(5);
		
		resolveDownIsLogic(node, set);
		
		return set;
	}

	private void resolveDownIsLogic(NodeProxy node, NodeSet set) {
		if (!downIsRelations.containsKey(node))
			return;
		
		for (NodeProxy n : downIsRelations.get(node)) {
			set.add(n);
			resolveDownIsLogic(n, set);
		}
	}
	
}
