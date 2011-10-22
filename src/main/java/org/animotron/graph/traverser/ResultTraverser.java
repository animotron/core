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
import org.animotron.statement.operator.*;
import org.animotron.statement.relation.HAVE;
import org.animotron.statement.relation.Relation;
import org.neo4j.graphdb.Relationship;

import java.io.IOException;
import java.util.Iterator;

import static org.animotron.Properties.RID;
import static org.animotron.graph.AnimoGraph.getDb;
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

    public void traverse(GraphHandler handler, Relationship r) throws IOException {
        handler.startGraph();
        build(handler, new PFlow(Evaluator._, r), r, 0, true);
        handler.endGraph();
    }

    public void traverse(PFlow pf, GraphHandler handler, Relationship r) throws IOException {
        handler.startGraph();
        pf.addContextPoint(r);
        build(handler, pf, r, 0, true);
        pf.popContextPoint();
        handler.endGraph();
    }

    @Override
    protected void build(GraphHandler handler, PFlow pf, Relationship r, int level, boolean isOne) throws IOException {

        int addedContexts = 0;
//        try {
//        	Relationship context = getDb().getRelationshipById(
//                (Long)r.getProperty(CID.name())
//            );
//            pf.addContextPoint(context);
//            addedContexts++;
//        } catch (Exception e) {
//		}

        if (r.isType(RESULT)) {
        	r = getDb().getRelationshipById(
                (Long) r.getProperty(RID.name())
            );
            pf.addContextPoint(r);
            addedContexts++;
        }
        
        Statement s;
        if (r.isType(REF) || r.isType(THE._)) {
            s = THE._;
        } else {
            s = Statements.relationshipType(r);
        }
        
        if (s instanceof Reference || s instanceof HAVE) {
	        pf.addContextPoint(r);
	        addedContexts++;
        }

        if (s != null) {
            if (s instanceof Query || s instanceof Evaluable) {
                result(handler, pf, r, level, isOne);
			} else if (!(s instanceof Relation)) {
                if (s instanceof Result)
                    handler.start(s, r, level++, isOne);
                node = r.getEndNode();
                It it = new It(node);
                try {
                    iterate(handler, pf, it, level, 0);
                } finally {
                    it.remove();
                }
                if (s instanceof Result)
                    handler.end(s, r, --level, isOne);
            }
        }

        while (addedContexts > 0) {
        	pf.popContextPoint();
        	addedContexts--;
        }
    }

    protected boolean result(GraphHandler handler, PFlow pf, Relationship r, int level, boolean isOne) throws IOException {
    	PFlow pflow = new PFlow(pf);
    	Iterator<Relationship> i = AnimoGraph.getResult(pflow.getLastContext(), r.getEndNode());
    	//Iterator<Relationship> i = r.getEndNode().getRelationships(RESULT, OUTGOING).iterator();
        boolean found = iterate(handler, pflow, i, level, isOne);
        if (!found) {
            //UNDERSTAND: calculate current r!
            //System.out.println("READER Execute r = "+r);
            PipedInput in = null;
            in = Evaluator._.execute(pflow, r);
            iterate(handler, pflow, in, level, isOne);
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
