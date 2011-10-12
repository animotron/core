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

import org.animotron.utils.MessageDigester;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.index.Index;
import org.neo4j.graphdb.index.IndexHits;
import org.neo4j.graphdb.index.IndexManager;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class CacheIndex {

	public static final String NAME = "CACHE";

	protected final Index<Node> INDEX;

	public CacheIndex(IndexManager indexManager) {
        INDEX = indexManager. forNodes(NAME);
//        INDEX = indexManager.forNodes(NAME, BerkeleyDbIndexImplementation.DEFAULT_CONFIG);

//		DatabaseManager dbm = AnimoGraph.getBabuDB().getDatabaseManager();
//        if (dbm.getDatabases().containsKey("myDB"))
//		    db = dbm.getDatabase("myDB");
//        else
//            db = dbm.createDatabase("myDB",32);
	}

    private Node get(IndexHits<Node> q) {
        Node n = null;
        try {
            n = q.next();
        } finally {
            q.close();
            return n;
        }
    }

    public Node get(byte[] value) {
        return get(INDEX.get(NAME, MessageDigester.byteArrayToHex(value)));
//        return get(IndexHits<Node> q = INDEX.get(NAME, value));
    }
    public Node get(String value) {
        return  get(INDEX.get(NAME, value));
    }

    public void add(Node n, byte[] value) {
        INDEX.add(n, NAME, MessageDigester.byteArrayToHex(value));
//        INDEX.add(n, NAME, value);
    }
    public void add(Node n, String value) {
        INDEX.add(n, NAME, value);
    }

}