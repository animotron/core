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
import org.animotron.manipulator.PFlow;
import org.animotron.statement.Statement;
import org.animotron.statement.Statements;
import org.animotron.statement.ml.NAME;
import org.animotron.statement.relation.Relation;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.index.IndexHits;

import java.io.IOException;
import java.util.Iterator;

import static org.animotron.graph.AnimoGraph.getORDER;
import static org.neo4j.graphdb.Direction.OUTGOING;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovskyd@gmail.com">Evgeny Gazdovsky</a>
 * 
 */
public class AnimoTraverser {

    public static AnimoTraverser _ = new AnimoTraverser();

    protected AnimoTraverser() {}
	
	public void traverse(GraphHandler handler, PFlow pf, Relationship r) throws IOException {
		handler.startGraph();
		build(handler, pf, r, 0, true);
		handler.endGraph();
	}
	
	protected void build(GraphHandler handler, PFlow pf, Relationship r, int level, boolean isOne) throws IOException {
		Statement statement = Statements.relationshipType(r.getType());
		if (statement == null)
			return;
		handler.start(statement, r, level++, isOne);
		if (!(statement instanceof Relation)) {
            Node node = r.getEndNode();
            IndexHits<Relationship> q = getORDER().query(node);
			try {
                int size = q.size();
                if (r.getEndNode().hasRelationship(NAME._.relationshipType(), OUTGOING)) size--;
                iterate(handler, pf, q.iterator(), level, size);
			} finally {
				q.close();
			}
		}
		handler.end(statement, r, --level, isOne);
	}

    protected void iterate(GraphHandler handler, PFlow pf, Iterator<Relationship> it, int level, int count) throws IOException {
        boolean isOne = count < 2;
        while (it.hasNext()) {
            Relationship i = it.next();
            build(handler, new PFlow(pf, i), i, level, isOne);
        }
    }

    public static class It implements Iterator <Object[]>, Iterable<Object[]> {

        private Node n;
        private Iterator<String> p;
        private Iterator<Relationship> r;
        private IndexHits<Relationship> q;

        private Next c;

        public It (Node node) {
            n = node;
            p = n.getPropertyKeys().iterator();
            c = P;
        }

        @Override
        public Iterator<Object[]> iterator() {
            return this;
        }

        @Override
        public boolean hasNext() {
            if (p.hasNext())
                return true;
            q = getORDER().query(n);
            r = q.iterator();
            c = R;
            return r.hasNext();
        }

        @Override
        public Object[] next() {
            return c.next();
        }

        @Override
        public void remove() {
            q.close();
        }

        private interface Next {
            public Object[] next();
        }

        private final Next P = new Next(){
            @Override
            public Object[] next() {
                String name = p.next();
                Object[] o = {
                    Statements.name(name),
                    n.getProperty(name)
                };
                return o;
            }
        };

        private final Next R = new Next(){
            @Override
            public Object[] next() {
                Relationship op = r.next();
                Object[] o = {
                    Statements.relationshipType(op),
                    op
                };
                return o;
            }
        };

    }
	
}
