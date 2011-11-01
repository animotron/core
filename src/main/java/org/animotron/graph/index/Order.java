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
package org.animotron.graph.index;

import org.apache.lucene.search.Sort;
import org.apache.lucene.search.SortField;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.index.IndexHits;
import org.neo4j.graphdb.index.IndexManager;
import org.neo4j.graphdb.index.RelationshipIndex;
import org.neo4j.index.lucene.QueryContext;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class Order {
	
	public static final String NAME = "order";
	
//	private static final QueryContext SORT;
//	static {
//		QueryContext q = new QueryContext( "*" );
//        SortField[] sortFields = new SortField[1];
//        sortFields[0] = new SortField(NAME, SortField.LONG );
//        SORT = q.sort( new Sort( sortFields ) );
//	}

	private static RelationshipIndex INDEX;

	public static void init(IndexManager indexManager) {
		INDEX = indexManager.forRelationships(NAME);
	}

	private static QueryContext sort( String key, String... additionalKeys ) {
		QueryContext q = new QueryContext( "*" );
        SortField firstSortField = new SortField( key, SortField.LONG );
        if ( additionalKeys.length == 0 ) {
            return q.sort( new Sort( firstSortField ) );
        }
        SortField[] sortFields = new SortField[1+additionalKeys.length];
        sortFields[0] = firstSortField;
        for ( int i = 0; i < additionalKeys.length; i++ ) {
            sortFields[1+i] = new SortField( additionalKeys[i], SortField.LONG );
        }
        return q.sort( new Sort( sortFields ) );
    }

    public static void order(Relationship r, int value) {
        INDEX.add(r, NAME, value);
        //r.setProperty(NAME, value);
    }
	
    public static void unorder(Relationship r) {
        INDEX.remove(r, NAME);
    }

    public static IndexHits<Relationship> queryDown(Node node) {
        return INDEX.query(NAME, sort(NAME), node, null);
    }

    public static IndexHits<Relationship> queryUp(Node node) {
        return INDEX.query(NAME, sort(NAME), null, node);
    }

//	public static Relationship position(long position, Node startNode) {
//		IndexHits<Relationship> q = queryDown(startNode);
//		try {
//			if (q.hasNext()) return q.next();
//		} finally {
//			q.close();
//		}
//		return null;
//	}

	public static Relationship[] first(int qty, Node startNode) {
		IndexHits<Relationship> q = queryDown(startNode);
		try {
			Relationship[] res = new Relationship[qty];
			for (int i = 0; i < qty; i++) {
				if (q.hasNext()) res[i] = q.next();
				else throw new IndexOutOfBoundsException("Required: "+qty+", have: "+(i+1));
			}
			return res;
		} finally {
			q.close();
		}
	}

}