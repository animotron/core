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
import org.neo4j.graphdb.PropertyContainer;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.index.Index;
import org.neo4j.graphdb.index.IndexHits;
import org.neo4j.graphdb.index.IndexManager;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class Cache {

	public static final String NAME = "CACHE";

    private static CacheIndex<Node> NCACHE;
    private static CacheIndex<Relationship> RCACHE;

	public static void init (IndexManager indexManager) {
        NCACHE = new CacheIndex<Node>(indexManager) {
            public void init(IndexManager indexManager) {
                INDEX = indexManager.forNodes(NAME);
            }
        };
        RCACHE = new CacheIndex<Relationship>(indexManager) {
            public void init(IndexManager indexManager) {
                INDEX = indexManager.forRelationships(NAME);
            }
        };
	}

    public static Node getNode(Object value) {
        return NCACHE.get(value);
    }

    public static Relationship getRelationship(Object value) {
        return RCACHE.get(value);
    }

    public static void removeNode(Node n, Object value) {
        NCACHE.remove(n, value);
    }

    public static void removeNode(Node n) {
        NCACHE.remove(n);
    }

    public static void putNode(Node n, Object value) {
        NCACHE.put(n, value);
    }

    public static void putRelationship(Relationship r, Object value) {
        RCACHE.put(r, value);
    }

    public static void removeRelationship(Relationship r, Object value) {
        RCACHE.remove(r, value);
    }

    public static void removeRelationship(Relationship r) {
        RCACHE.remove(r);
    }

    public static Object key(Object value) {
        if (value instanceof byte[])
            return MessageDigester.byteArrayToHex((byte[]) value);
        return value;
    }

    private abstract static class CacheIndex<T extends PropertyContainer> {

        protected Index<T> INDEX;

        public CacheIndex(IndexManager indexManager){
            init(indexManager);
        }

        public abstract void init(IndexManager indexManager);

        public T get(Object value) {
            IndexHits<T> q = INDEX.get(NAME, key(value));
            T c = null;
            try {
                c = q.next();
            } finally {
                q.close();
                return c;
            }
        }

        public void put(T c, Object value) {
            INDEX.add(c, NAME, key(value));
        }

        public void remove(T c, Object value) {
            INDEX.remove(c, NAME, key(value));
        }

        public void remove(T c) {
            INDEX.remove(c, NAME);
        }

    }

}