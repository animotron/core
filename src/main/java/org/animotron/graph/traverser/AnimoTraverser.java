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

import javolution.util.FastList;
import org.animotron.graph.handler.GraphHandler;
import org.animotron.manipulator.Evaluator;
import org.animotron.manipulator.PFlow;
import org.animotron.manipulator.QCAVector;
import org.animotron.statement.Statement;
import org.animotron.statement.Statements;
import org.animotron.statement.ml.QNAME;
import org.animotron.statement.operator.REF;
import org.animotron.statement.relation.USE;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;

import java.io.IOException;
import java.util.List;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovskyd@gmail.com">Evgeny Gazdovsky</a>
 * 
 */
public class AnimoTraverser {

    public static AnimoTraverser _ = new AnimoTraverser();
    protected Node node;

    protected AnimoTraverser() {}
	
    public void traverse(GraphHandler handler, Relationship r) throws IOException {
        handler.startGraph();
        build(handler, null, null, r, 0, true, 0, true);
        handler.endGraph();
    }

	public void traverse(GraphHandler handler, PFlow pf, Relationship r) throws IOException {
		handler.startGraph();
		build(handler, pf, null, r, 0, true, 0, true);
		handler.endGraph();
	}
	
    public void traverse(GraphHandler handler, QCAVector vector) throws IOException {
        handler.startGraph();
        build(handler, new PFlow(Evaluator._), null, vector, 0, true, 0, true);
        handler.endGraph();
    }

    public void traverse(GraphHandler handler, PFlow pf, QCAVector vector) throws IOException {
        handler.startGraph();
        build(handler, pf, null, vector, 0, true, 0, true);
        handler.endGraph();
    }

	protected void build(GraphHandler handler, PFlow pf, Statement parent, Object o, int level, boolean isOne, int pos, boolean isLast) throws IOException {
        if (o instanceof Relationship) {
            build(handler, pf, parent, new QCAVector((Relationship)o), level, isOne, pos, isLast);

        } else if (o instanceof QCAVector) {
            build(handler, pf, parent, (QCAVector)o, level, isOne, pos, isLast);
            
        } else {
            String name = (String) o;
            Statement statement = Statements.name(name);
            String reference = (String) node.getProperty(name);
            handler.start(statement, parent, reference, level, isOne, pos, isLast);
            handler.end(statement, parent, reference, level, isOne, pos, isLast);
        }
    }

	protected void build(GraphHandler handler, PFlow pf, Statement parent, QCAVector rr, int level, boolean isOne, int pos, boolean isLast) throws IOException {
		Relationship r = rr.getClosest();
		Statement statement = Statements.relationshipType(r);
		if (statement == null)
			return;
		handler.start(statement, parent, r, level++, isOne, pos, isLast);
		if (!(statement instanceof USE || statement instanceof REF)) {
            node = r.getEndNode();
            It it = new It(node);
            iterate(handler, pf, statement, it, level);
		}
		handler.end(statement, parent, r, --level, isOne, pos, isLast);
	}

    protected void iterate(GraphHandler handler, PFlow pf, Statement parent, It it, int level) throws IOException {
        List<Object> o = new FastList<Object>();
        try {
            int count = 0;
            while (it.hasNext()) {
                Object i = it.next();
                o.add(i);
                if (i instanceof Relationship) {
                    Relationship r = (Relationship) i;
                    if (!(r.isType(REF._) || r.isType(QNAME._))) {
                        break;
                    }
                } else if (!i.equals(QNAME._.name())) {
                    break;
                }
                count++;
            }
            int n = 0;
            while (it.hasNext() && n < 2) {
                o.add(it.next());
                n++;
            }
            int pos = 0;
            boolean isOne = o.size() - count < 2;
            for (Object i : o) {
                boolean isLast = pos < o.size() - 1 ? false : !it.hasNext();
                build(handler, pf, parent, i, level, isOne, pos, isLast);
                pos++;
            }
            while (it.hasNext()) {
                build(handler, pf, parent, it.next(), level, isOne, pos++, !it.hasNext());
            }
        } finally {
            it.remove();
        }
    }

}
