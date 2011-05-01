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

import java.io.File;

import org.apache.log4j.Logger;
import org.exist.dom.DocumentImpl;
import org.exist.indexing.AbstractIndex;
import org.exist.indexing.IndexWorker;
import org.exist.numbering.DLN;
import org.exist.numbering.NodeId;
import org.exist.storage.BrokerPool;
import org.exist.storage.DBBroker;
import org.exist.storage.btree.DBException;
import org.exist.util.DatabaseConfigurationException;
import org.exist.xmldb.XmldbURI;
import org.neo4j.graphdb.GraphDatabaseService;
import org.neo4j.index.IndexService;
import org.neo4j.index.lucene.LuceneIndexService;
import org.neo4j.kernel.EmbeddedGraphDatabase;
import org.w3c.dom.Element;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * 
 */
public class AnimoIndex extends AbstractIndex {

	protected static final Logger LOG = Logger.getLogger(AnimoIndex.class);

	public static final String ID = AnimoIndex.class.getName();
	public static final String FOLDER_NAME = "animo";

	protected GraphDatabaseService graphDb;
	protected IndexService indexService;

	protected DocumentImpl unresolvedReferenceDocument = null;
	protected NodeId unresolvedReferenceId = new DLN();

	public final static XmldbURI ANIMO_COLLETION_URI = 
		XmldbURI.SYSTEM_COLLECTION_URI.append("animo");
	
	public final static XmldbURI UNRESOLVED_REFERENCES_FILE_URI = 
		XmldbURI.create("unresolved-references.xml");

	public void configure(BrokerPool db, String dataDir, Element config) throws DatabaseConfigurationException {
		super.configure(db, dataDir, config);

		// initialize(db);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.exist.indexing.AbstractIndex#open()
	 */
	@Override
	public void open() throws DatabaseConfigurationException {
		File folder = new File(getDataDir(), FOLDER_NAME);
		
		graphDb = new EmbeddedGraphDatabase(folder.getAbsolutePath());
		indexService = new LuceneIndexService( graphDb );

		LOG.debug("Animo index opened at '" + folder.getAbsolutePath() + "' .");
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.exist.indexing.AbstractIndex#close()
	 */
	@Override
	public void close() throws DBException {
		graphDb.shutdown();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.exist.indexing.AbstractIndex#sync()
	 */
	@Override
	public void sync() throws DBException {
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.exist.indexing.AbstractIndex#remove()
	 */
	@Override
	public void remove() throws DBException {
		graphDb.shutdown();

		File folder = new File(getDataDir(), FOLDER_NAME);
		deleteFolder(folder);

	}

	private boolean deleteFolder(File path) {
		if (path.exists()) {
			File[] files = path.listFiles();
			for (int i = 0; i < files.length; i++) {
				if (files[i].isDirectory()) {
					deleteFolder(files[i]);
				} else {
					files[i].delete();
				}
			}
		}
		return path.delete();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.exist.indexing.AbstractIndex#getWorker(org.exist.storage.DBBroker)
	 */
	@Override
	public IndexWorker getWorker(DBBroker broker) {
		return new AnimoIndexWorker(this);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.exist.indexing.AbstractIndex#checkIndex(org.exist.storage.DBBroker)
	 */
	@Override
	public boolean checkIndex(DBBroker broker) {
		try {
			Class.forName("org.neo4j.kernel.EmbeddedGraphDatabase");
		} catch (Exception e) {
			System.err.println("ERROR: failed to load neo4j.");
			return false;
		}
		return true;
	}

	// private void initialize(Database db) {
	//
	// if (unresolvedReferenceDocument != null) return;
	//
	// TransactionManager tm = db.getTransactionManager();
	//
	// DBBroker broker = null;
	// Txn txn = null;
	// try {
	// broker =
	// db.getActiveBroker();//db.getSecurityManager().getSystemSubject()
	//
	// Collection animoCollection = null;
	// try {
	// animoCollection = broker.getCollection(ANIMO_COLLETION_URI);
	// if (animoCollection == null) {
	// txn = tm.beginTransaction();
	// animoCollection = broker.getOrCreateCollection(txn, ANIMO_COLLETION_URI);
	// if (animoCollection == null)
	// return;
	// animoCollection.setPermissions(0770);
	// broker.saveCollection(txn, animoCollection);
	//
	// tm.commit(txn);
	// }
	// } catch (Exception e) {
	// tm.abort(txn);
	// e.printStackTrace();
	// //LOG.debug("loading acl failed: " + e.getMessage());
	// }
	//
	// unresolvedReferenceDocument = animoCollection.getDocument(broker,
	// UNRESOLVED_REFERENCES_FILE_URI);
	//
	// if (unresolvedReferenceDocument == null) {
	// txn = tm.beginTransaction();
	//
	// String data = "<unresolved-references/>";
	// IndexInfo info = animoCollection.validateXMLResource(txn, broker,
	// UNRESOLVED_REFERENCES_FILE_URI, data);
	// animoCollection.store(txn, broker, info, data, false);
	//
	// unresolvedReferenceDocument = info.getDocument();
	//
	// tm.commit(txn);
	// }
	// } catch (Exception e) {
	//
	// if (tm != null)
	// tm.abort(txn);
	//
	// e.printStackTrace();
	//
	// fail(e.getMessage());
	//
	// }
	//
	// unresolvedReference = (ElementImpl)
	// unresolvedReferenceDocument.getDocumentElement();
	// }
}
