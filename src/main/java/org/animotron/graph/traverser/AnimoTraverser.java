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

import org.animotron.graph.OrderIndex;
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

import static org.neo4j.graphdb.Direction.OUTGOING;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovskyd@gmail.com">Evgeny Gazdovsky</a>
 * 
 */
public class AnimoTraverser {

    public static AnimoTraverser _ = new AnimoTraverser();
    protected Node node;

    protected AnimoTraverser() {}
	
	public void traverse(GraphHandler handler, PFlow pf, Relationship r) throws IOException {
		handler.startGraph();
		build(handler, pf, r, 0, true);
		handler.endGraph();
	}
	
//    protected void build(GraphHandler handler, PFlow pf, Object o, int level, boolean isOne) throws IOException {
//        if (o instanceof Relationship) {
//            build(handler, pf, (Relationship) o, level, isOne);
//        }
//    }

	protected void build(GraphHandler handler, PFlow pf, Relationship r, int level, boolean isOne) throws IOException {
		Statement statement = Statements.relationshipType(r);
		if (statement == null)
			return;
		handler.start(statement, r, level++, isOne);
		if (!(statement instanceof Relation)) {
            node = r.getEndNode();
            iterate(handler, pf, new It(node), level, hasStatement(node, NAME._) ? 1 : 0);
		}
		handler.end(statement, r, --level, isOne);
	}

    protected void iterate(GraphHandler handler, PFlow pf, It it, int level, int count) throws IOException {
        boolean isOne = it.size() - count < 2;
        try {
            while (it.hasNext()) {
                build(handler, pf, it.next(), level, isOne);
            }
        } finally {
            it.remove();
        }
    }

    protected boolean hasStatement(Node node, Statement s) {
        return node.hasRelationship(s, OUTGOING);
    }

    protected class It implements Iterator <Relationship>, Iterable<Relationship> {

        private Iterator<Relationship> r;
        private IndexHits<Relationship> q;

        public It (Relationship r) {
            this(r.getEndNode());
        }

        public It (Node n) {
            q = OrderIndex.queryDown(n);
            r = q.iterator();
        }

        @Override
        public Iterator<Relationship> iterator() {
            return this;
        }

        @Override
        public boolean hasNext() {
            return r.hasNext();
        }

        @Override
        public Relationship next() {
            return r.next();
        }

        @Override
        public void remove() {
            q.close();
        }

        public int size() {
            return q.size();
        }

    }
}