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

import static org.animotron.graph.AnimoGraph.getORDER;

import org.animotron.Statement;
import org.animotron.Statements;
import org.animotron.operator.Relation;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.index.IndexHits;

/**
 * @author <a href="mailto:gazdovskyd@gmail.com">Evgeny Gazdovsky</a>
 * 
 */
public class GraphTraverse {
	
	protected static GraphTraverse _ = new GraphTraverse();

	private GraphTraverse() {}
	
	public void traverse(GraphHandler handler, Relationship r) {
		handler.startGraph();
		build(handler, r);
		handler.endGraph();
	}
	
	private void build(GraphHandler handler, Relationship r) {
		
		Statement statement = Statements.relationshipType(r.getType());
		
		if (statement == null)
			return;
		
		handler.start(statement, r);
		
		if (!(statement instanceof Relation)) {
			
			IndexHits<Relationship> q = getORDER().query(r.getEndNode());
			try {
				for (Relationship i : q) {
					//use pool here? well, it will use stack anyway ...
					build(handler, i);
				}
			} finally {
				q.close();
			}
		}

		handler.end(statement, r);
	}
	
}
