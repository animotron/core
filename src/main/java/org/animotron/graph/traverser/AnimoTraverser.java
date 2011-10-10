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

import org.animotron.graph.RelationshipTypes;
import org.animotron.graph.handler.GraphHandler;
import org.animotron.manipulator.PFlow;
import org.animotron.statement.Statement;
import org.animotron.statement.Statements;
import org.animotron.statement.ml.NAME;
import org.animotron.statement.relation.Relation;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;

import java.io.IOException;
import java.util.Iterator;

import static org.animotron.Properties.CID;
import static org.animotron.Properties.RID;
import static org.animotron.Properties.VALUE;
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
	
    protected void build(GraphHandler handler, PFlow pf, Object o, int level, boolean isOne) throws IOException {
        if (o instanceof Relationship) {
            build(handler, pf, (Relationship) o, level, isOne);
        } else {
            String name = (String) o;
            Statement statement = Statements.name(name);
            String reference = (String) node.getProperty(name);
            handler.start(statement, reference, level, isOne);
            handler.end(statement, reference, level, isOne);
        }
    }

	protected void build(GraphHandler handler, PFlow pf, Relationship r, int level, boolean isOne) throws IOException {
		Statement statement = Statements.relationshipType(r);
		if (statement == null)
			return;
		handler.start(statement, r, level++, isOne);
		if (!(statement instanceof Relation)) {
            node = r.getEndNode();
            It it = new It(node);
            int size = hasStatement(node, NAME._) ? 1 : 0;
			try {
                iterate(handler, pf, it, level, size);
			} finally {
				it.remove();
			}
		}
		handler.end(statement, r, --level, isOne);
	}

    protected void iterate(GraphHandler handler, PFlow pf, It it, int level, int count) throws IOException {
        int n = 0;
        Object[] o = {null, null, null};
        while (it.hasNext() && n < 3)
            o[n++] = it.next();
        boolean isOne = n - count < 2;
        for (int i = 0; i < n; i++)
            build(handler, pf, o[i], level, isOne);
        while (it.hasNext())
            build(handler, pf, it.next(), level, isOne);
    }

    protected boolean hasStatement(Node node, Statement s) {
        return node.hasProperty(s.name()) ||
               node.hasRelationship(s.relationshipType(), OUTGOING);
    }

    protected class It implements Iterator <Object>, Iterable<Object> {

        private Iterator<String> p;
        private Iterator<Relationship> r;

        Object current = null;

        public It (Relationship r) {
            this(r.getEndNode());
        }

        public It (Node n) {
            p = n.getPropertyKeys().iterator();
            r = n.getRelationships(OUTGOING).iterator();
            next();
        }

        @Override
        public Iterator<Object> iterator() {
            return this;
        }

        @Override
        public boolean hasNext() {
            return current != null;
        }

        @Override
        public Object next() {
            Object next = current;
            current = step1();
            return next;
        }

        private Object step1() {
            if (p.hasNext()) {
                String o = p.next();
                if (VALUE.name().equals(o)) {
                    return step1();
                } else return o;
            } else return step2();
        }
        private Object step2() {
            if (r.hasNext()) {
                Relationship o = r.next();
                if (o.isType(RelationshipTypes.REF) || CID.has(o) || RID.has(o)) {
                    return step2();
                } else return o;
            } else return null;
        }

        @Override
        public void remove() {
        }

    }
	
}
