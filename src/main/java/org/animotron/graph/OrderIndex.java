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
 *
 */
public class OrderIndex {
	
	public static final String ORDER = "ORDER";
	
//	private static final QueryContext SORT;
//	static {
//		QueryContext q = new QueryContext( "*" );
//        SortField[] sortFields = new SortField[1];
//        sortFields[0] = new SortField(ORDER, SortField.LONG );
//        SORT = q.sort( new Sort( sortFields ) );
//	}

	protected final RelationshipIndex ORDER_INDEX;
	
	public OrderIndex(IndexManager indexManager) {
		ORDER_INDEX = indexManager.forRelationships(ORDER);
	}

	protected QueryContext sort( String key, String... additionalKeys ) {
		QueryContext q = new QueryContext( "*" );
		
        SortField firstSortField = new SortField( key, SortField.LONG );
        if ( additionalKeys.length == 0 )
        {
            return q.sort( new Sort( firstSortField ) );
        }
        
        SortField[] sortFields = new SortField[1+additionalKeys.length];
        sortFields[0] = firstSortField;
        for ( int i = 0; i < additionalKeys.length; i++ )
        {
            sortFields[1+i] = new SortField( additionalKeys[i], SortField.LONG );
        }
        return q.sort( new Sort( sortFields ) );
    }

	public void add(Relationship r, int value) {
		ORDER_INDEX.add(r, ORDER, value);
	}
	
	public IndexHits<Relationship> query(Node startNode) {
		return ORDER_INDEX.query(ORDER, sort( ORDER ), startNode, null);
	}

	public Relationship position(long position, Node startNode) {
		IndexHits<Relationship> q = query(startNode);
		try {
			if (q.hasNext()) return q.next();
		} finally {
			q.close();
		}
		return null;
	}

	public Relationship[] first(int qty, Node startNode) {
		IndexHits<Relationship> q = query(startNode);
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