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

import org.animotron.Properties;
import org.animotron.Statement;
import org.animotron.Statements;
import org.animotron.operator.Relation;
import org.animotron.operator.THE;
import org.apache.lucene.search.Sort;
import org.apache.lucene.search.SortField;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.RelationshipType;
import org.neo4j.graphdb.index.IndexHits;
import org.neo4j.index.lucene.QueryContext;

/**
 * @author <a href="mailto:gazdovskyd@gmail.com">Evgeny Gazdovsky</a>
 * 
 */
public abstract class AbstractGraphSerializer implements GraphHandler {
	
	private static final String ORDER = Properties.ORDER.name();
	private static final QueryContext SORT;
	
	static {
		QueryContext q = new QueryContext( "*" );
        SortField[] sortFields = new SortField[1];
        sortFields[0] = new SortField(ORDER, SortField.LONG );
        SORT = q.sort( new Sort( sortFields ) );
	}
	
	final public void serialize(Relationship r) {
		startDocument();
		build(r);
		endDocument();
	}
	
	private void build(Relationship r) {
		
		RelationshipType type = r.getType();
		Statement statement = type.name().startsWith(THE.PREFIX + ":") ? THE._ : Statements.relationshipType(type);
		
		if (statement == null)
			return;
		
		start(statement, r);
		
		
		if (!(statement instanceof Relation)) {
			
			IndexHits<Relationship> q = AnimoGraph.ORDER.query(ORDER, SORT, r.getEndNode(), null);
			
			try {
				for (Relationship i : q) {
					build(i);
				}
			} finally {
				q.close();
			}
			
		}
		
		end(statement, r);
	}
	
}
