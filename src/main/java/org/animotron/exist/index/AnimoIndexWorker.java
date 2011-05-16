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

import org.exist.collections.Collection;
import org.exist.dom.AttrImpl;
import org.exist.dom.CharacterDataImpl;
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

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.exist.indexing.IndexWorker#getIndexId()
	 */
	public String getIndexId() {
		return AnimoIndex.ID;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.exist.indexing.IndexWorker#getIndexName()
	 */
	public String getIndexName() {
		return index.getIndexName();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.exist.indexing.IndexWorker#configure(org.exist.indexing.IndexController
	 * , org.w3c.dom.NodeList, java.util.Map)
	 */
	public Object configure(IndexController controller, NodeList configNodes,
			Map<String, String> namespaces)
			throws DatabaseConfigurationException {
		// TODO Auto-generated method stub
		return null;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.exist.indexing.IndexWorker#setDocument(org.exist.dom.DocumentImpl)
	 */
	public void setDocument(DocumentImpl doc) {
		this.document = doc;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.exist.indexing.IndexWorker#setDocument(org.exist.dom.DocumentImpl,
	 * int)
	 */
	public void setDocument(DocumentImpl doc, int mode) {
		this.document = doc;
		this.mode = mode;
		System.out.println("setDocument doc = " + doc.getDocumentURI() + " mode = " + mode);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.exist.indexing.IndexWorker#setMode(int)
	 */
	public void setMode(int mode) {
		this.mode = mode;
		System.out.println("setMode mode = " + mode);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.exist.indexing.IndexWorker#getDocument()
	 */
	public DocumentImpl getDocument() {
		return document;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.exist.indexing.IndexWorker#getMode()
	 */
	public int getMode() {
		return mode;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.exist.indexing.IndexWorker#getReindexRoot(org.exist.dom.StoredNode,
	 * org.exist.storage.NodePath, boolean)
	 */
	public StoredNode getReindexRoot(StoredNode node, NodePath path,
			boolean includeSelf) {
		System.out.println("getReindexRoot path = " + path);
		// TODO Auto-generated method stub
		return null;
	}

	private StreamListener listener = new AnimoStreamListener();

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.exist.indexing.IndexWorker#getListener()
	 */
	public StreamListener getListener() {
		return listener;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.exist.indexing.IndexWorker#getMatchListener(org.exist.storage.DBBroker
	 * , org.exist.dom.NodeProxy)
	 */
	public MatchListener getMatchListener(DBBroker broker, NodeProxy proxy) {
		// TODO Auto-generated method stub
		return null;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.exist.indexing.IndexWorker#flush()
	 */
	public void flush() {

		// TODO Auto-generated method stub

	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.exist.indexing.IndexWorker#removeCollection(org.exist.collections
	 * .Collection, org.exist.storage.DBBroker)
	 */
	public void removeCollection(Collection collection, DBBroker broker) {
		System.out.println("removeCollection " + collection);
		// TODO Auto-generated method stub

	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.exist.indexing.IndexWorker#checkIndex(org.exist.storage.DBBroker)
	 */
	public boolean checkIndex(DBBroker broker) {
		System.out.println("checkIndex called");
		// TODO Auto-generated method stub
		return false;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.exist.indexing.IndexWorker#scanIndex(org.exist.xquery.XQueryContext,
	 * org.exist.dom.DocumentSet, org.exist.dom.NodeSet, java.util.Map)
	 */
	public Occurrences[] scanIndex(XQueryContext context, DocumentSet docs, NodeSet contextSet, Map hints) {
		// TODO Auto-generated method stub
		return null;
	}

	private class AnimoStreamListener extends AbstractStreamListener {

		private AnimoGraphBuilder builder = new AnimoGraphBuilder();

		@Override
		public void startElement(Txn transaction, ElementImpl element, NodePath path) {
			if (mode == STORE) {
				builder.startElement(element);
			} else {
				System.out.println("mode = " + mode + " path = " + path);
			}
			super.startElement(transaction, element, path);
		}

		@Override
		public void endElement(Txn transaction, ElementImpl element, NodePath path) {
			if (mode == STORE) {
				builder.endElement(element);
			}
			super.endElement(transaction, element, path);

		}

	    @Override
	    public void attribute(Txn transaction, AttrImpl attribute, NodePath path) {
			if (mode == STORE) {
				builder.attribute(attribute);
			}
			super.attribute(transaction, attribute, path);
	    }
	    
	    @Override
	    public void characters(Txn transaction, CharacterDataImpl text, NodePath path) {
			if (mode == STORE) {
				builder.characters(text);
			}
			super.characters(transaction, text, path);
	    }

		@Override
		public IndexWorker getWorker() {
			return AnimoIndexWorker.this;
		}

	}

}
