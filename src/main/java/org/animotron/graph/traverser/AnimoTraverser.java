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
package org.animotron.graph.traverser;

import org.animotron.graph.handler.GraphHandler;
import org.animotron.statement.Statement;
import org.animotron.statement.Statements;
import org.animotron.statement.relation.Relation;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.index.IndexHits;

import java.io.IOException;
import java.util.Iterator;

import static org.animotron.graph.AnimoGraph.getORDER;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovskyd@gmail.com">Evgeny Gazdovsky</a>
 * 
 */
public class AnimoTraverser {

    public static AnimoTraverser _ = new AnimoTraverser();

    protected AnimoTraverser() {}
	
	public void traverse(GraphHandler handler, Relationship r) throws IOException {
		handler.startGraph();
		build(handler, r, 0, true);
		handler.endGraph();
	}
	
	protected void build(GraphHandler handler, Relationship r, int level, boolean isOne) throws IOException {
		Statement statement = Statements.relationshipType(r.getType());
		if (statement == null)
			return;
		handler.start(statement, r, level++, isOne);
		if (!(statement instanceof Relation)) {
            IndexHits<Relationship> q = getORDER().query(r.getEndNode());
            Iterator<Relationship> it = q.iterator();
			try {
                iterate(handler, it, level);
			} finally {
				q.close();
			}
		}
		handler.end(statement, r, level--, isOne);
	}

    protected void iterate(GraphHandler handler, Iterator<Relationship> it, int level) throws IOException {
        boolean isFirst = true;
        while (it.hasNext()) {
            Relationship i = it.next();
            if (isFirst) {
                if (it.hasNext()) {
                    build(handler, i, level, false);
                    build(handler, it.next(), level, false);
                } else {
                    build(handler, i, level, true);
                }
            } else {
                build(handler, i, level, false);
            }
            isFirst = false;
        }
    }
	
}
