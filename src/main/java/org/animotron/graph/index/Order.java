/*
 *  Copyright (C) 2011-2012 The Animo Project
 *  http://animotron.org
 *  	
 *  This file is part of Animotron.
 *  
 *  Animotron is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU Affero General Public License as 
 *  published by the Free Software Foundation, either version 3 of 
 *  the License, or (at your option) any later version.
 *  
 *  Animotron is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Affero General Public License for more details.
 *  
 *  You should have received a copy of 
 *  the GNU Affero General Public License along with Animotron.  
 *  If not, see <http://www.gnu.org/licenses/>.
 */
package org.animotron.graph.index;

import java.util.Iterator;

import org.apache.lucene.search.Sort;
import org.apache.lucene.search.SortField;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.index.IndexHits;
import org.neo4j.graphdb.index.IndexManager;
import org.neo4j.index.lucene.QueryContext;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class Order extends AbstractRelationshipIndex {
	
	public static Order _ = new Order("order");

    private Order(String name) {super(name);}

//	private static final QueryContext SORT;
//	static {
//		QueryContext q = new QueryContext( "*" );
//        SortField[] sortFields = new SortField[1];
//        sortFields[0] = new SortField(NAME, SortField.LONG );
//        SORT = q.sort( new Sort( sortFields ) );
//	}

    @Override
	public void init(IndexManager index) {
		init(index.forRelationships(name));
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


    public IndexHits<Relationship> queryDown(Node node) {
        return index().query(name, sort(name), node, null);
    }

    public Iterable<Relationship> queryDownIterable(final Node node) {
    	return new Iterable<Relationship>() {

			@Override
			public Iterator<Relationship> iterator() {
				return new Iterator<Relationship>() {
					
					IndexHits<Relationship> hits = queryDown(node);

					@Override
					public boolean hasNext() {
						if (!hits.hasNext()) {
							hits.close();
						}

						return hits.hasNext();
					}

					@Override
					public Relationship next() {
						final Relationship r = hits.next();
						System.out.println("next: "+r);
						return r;
					}

					@Override
					public void remove() {
					}
					
				};
			}
    		
    	};
    }

    @Deprecated //not safe to use
    public IndexHits<Relationship> queryUp(Node node) {
        return index().query(name, sort(name), null, node);
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

	public Relationship[] first(int qty, Node node) {
		IndexHits<Relationship> q = queryDown(node);
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

    public IndexHits<Relationship> context(Node node) {
        return new ContextHits(node, index().query(name, sort(name), node, null));
    }
}