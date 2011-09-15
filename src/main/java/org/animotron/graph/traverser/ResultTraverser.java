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

import org.animotron.graph.AnimoGraph;
import org.animotron.graph.handler.GraphHandler;
import org.animotron.io.PipedInput;
import org.animotron.manipulator.Evaluator;
import org.animotron.manipulator.PFlow;
import org.animotron.statement.Statement;
import org.animotron.statement.Statements;
import org.animotron.statement.operator.Evaluable;
import org.animotron.statement.operator.Query;
import org.animotron.statement.operator.Result;
import org.animotron.statement.operator.THE;
import org.animotron.statement.relation.IS;
import org.animotron.statement.relation.USE;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.RelationshipType;
import org.neo4j.graphdb.index.IndexHits;

import java.io.IOException;
import java.util.Iterator;

import static org.animotron.Properties.RID;
import static org.animotron.graph.AnimoGraph.getDb;
import static org.animotron.graph.AnimoGraph.getORDER;
import static org.animotron.graph.RelationshipTypes.REF;
import static org.animotron.graph.RelationshipTypes.RESULT;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovskyd@gmail.com">Evgeny Gazdovsky</a>
 * 
 */
public class ResultTraverser extends AnimoTraverser {

    public static ResultTraverser _ = new ResultTraverser();

    protected ResultTraverser() {}

    public void traverse(GraphHandler handler, Relationship start_op, Relationship r) throws IOException {
        handler.startGraph();
        build(handler, new PFlow(Evaluator._, start_op, r), r, 0, true);
        handler.endGraph();
    }

    @Override
    protected void build(GraphHandler handler, PFlow pf, Relationship r, int level, boolean isOne) throws IOException {

        RelationshipType type = r.getType();
        String typeName = type.name();

        if (RESULT.name().equals(typeName)) {
            r = getDb().getRelationshipById(
                    (Long)r.getProperty(RID.name())
                );

            type = r.getType();
            typeName = type.name();
        }
        
        Statement s;
        PFlow pflow = pf;
        if (REF.equals(r)|| typeName.startsWith(THE._.name())) {
            s = THE._;
            pflow = new PFlow(pf, r);
        } else {
            s = Statements.relationshipType(typeName);
        }

        if (s != null) {
            if (s instanceof Query || s instanceof Evaluable) {
                result(handler, pf, r, level, isOne);
			} else if (!(s instanceof IS || s instanceof USE)) {
                if (s instanceof Result)
                    handler.start(s, r, level++, isOne);
                IndexHits<Relationship> q = getORDER().query(r.getEndNode());
                try {
                    iterate(handler, pf, q.iterator(), level, q.size());
                } finally {
                    q.close();
                }
                if (s instanceof Result)
                    handler.end(s, r, --level, isOne);
            }
        }

    }

    protected boolean result(GraphHandler handler, PFlow pf, Relationship r, int level, boolean isOne) throws IOException {
    	Iterator<Relationship> i = AnimoGraph.getResult(pf.getLastContext(), r.getEndNode());
    	//Iterator<Relationship> i = r.getEndNode().getRelationships(RESULT, OUTGOING).iterator();
        boolean found = iterate(handler, pf, i, level, isOne);
        if (!found) {
            //UNDERSTAND: calculate current r!
            //System.out.println("READER Execute r = "+r);
            PipedInput in = null;
            in = Evaluator._.execute(pf, r);
            iterate(handler, pf, in, level, isOne);
        }

        return found;

    }

    protected boolean iterate(GraphHandler handler, PFlow pf, Iterator<Relationship> it, int level, boolean isOne) throws IOException {
        boolean found = false;
        boolean isFirst = isOne;
        while (it.hasNext()) {
            Relationship i = it.next();
            if (isFirst) {
                if (it.hasNext()) {
                    build(handler, pf, i, level, false);
                    i = it.next();
                    build(handler, pf, i, level, false);
                } else {
                    build(handler, pf, i, level, true);
                }
            } else {
                build(handler, pf, i, level, false);
            }
            isFirst = false;
            found = true;
        }
        return found;
    }

    private void iterate(GraphHandler handler, PFlow pf, PipedInput in, int level, boolean isOne) throws IOException {
        Iterator<Object> it = in.iterator();
        boolean isFirst = isOne;
        while (it.hasNext()) {
            Relationship i = (Relationship) it.next();
            if (isFirst) {
                if (it.hasNext()) {
                    build(handler, pf, i, level, false);
                    i = (Relationship) it.next();
                    build(handler, pf, i, level, false);
                } else {
                    build(handler, pf, i, level, true);
                }
            } else {
                build(handler, pf, i, level, false);
            }
            isFirst = false;
        }
    }

}
