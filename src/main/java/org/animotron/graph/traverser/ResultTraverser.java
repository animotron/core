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
import org.animotron.graph.index.Result;
import org.animotron.io.PipedInput;
import org.animotron.manipulator.Evaluator;
import org.animotron.manipulator.PFlow;
import org.animotron.statement.Statement;
import org.animotron.statement.Statements;
import org.animotron.statement.operator.*;
import org.animotron.statement.relation.HAVE;
import org.animotron.statement.relation.Relation;
import org.animotron.statement.value.AbstractValue;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.index.IndexHits;

import java.io.IOException;
import java.util.Iterator;

import static org.animotron.Properties.CID;
import static org.animotron.Properties.RID;
import static org.animotron.graph.AnimoGraph.getDb;
import static org.animotron.graph.RelationshipTypes.RESULT;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovskyd@gmail.com">Evgeny Gazdovsky</a>
 * 
 */
public class ResultTraverser extends AnimoTraverser {

    public static ResultTraverser _ = new ResultTraverser();

    protected ResultTraverser() {}

    @Override
    public void traverse(GraphHandler handler, Relationship r) throws IOException {
        handler.startGraph();
        build(handler, new PFlow(Evaluator._, r), r, 0, true, 0, true);
        handler.endGraph();
    }

    @Override
    public void traverse(GraphHandler handler, PFlow pf, Relationship r) throws IOException {
        handler.startGraph();
        int i = pf.addContextPoint(r);
        build(handler, pf, r, 0, true, 0, true);
        if (i == 1) pf.popContextPoint();
        handler.endGraph();
    }

    @Override
    protected void build(GraphHandler handler, PFlow pf, Relationship r, int level, boolean isOne, int pos, boolean isLast) throws IOException {

        int addedContexts = 0;

        if (r.isType(RESULT)) {
        	r = getDb().getRelationshipById(
                (Long) r.getProperty(RID.name())
            );
            addedContexts += pf.addContextPoint(r);
        }

        Statement s;
        if (r.isType(RelationshipTypes.REF) || r.isType(THE._)) {
            s = THE._;
        } else {
            s = Statements.relationshipType(r);
        }
        
        if (s instanceof Reference || s instanceof HAVE)
	        addedContexts += pf.addContextPoint(r);

        try {
        	Relationship context = getDb().getRelationshipById(
        		(Long)r.getProperty(CID.name())
        	);
        	addedContexts += pf.addContextPoint(context);
        } catch (Exception e) {
        }
        
        process(handler, pf, s, r, level, isOne, 0, false);

        while (addedContexts > 0) {
        	pf.popContextPoint();
        	addedContexts--;
        }
    }
    
    protected void process(GraphHandler handler, PFlow pf, Statement s, Relationship r, int level, boolean isOne, int pos, boolean isLast) throws IOException {
        if (s != null) {
            if (s instanceof Query || s instanceof Evaluable) {
                result(handler, pf, r, level, isOne);
			} else if (!(s instanceof Relation || s instanceof REF)) {
                if (s instanceof AbstractValue)
                    handler.start(s, r, level++, isOne, pos, isLast);
                
                if (!(s instanceof REF)) {
	                node = r.getEndNode();
	                iterate(handler, pf, new It(node), level);
                }
                
                if (s instanceof AbstractValue)
                    handler.end(s, r, --level, isOne, pos, isLast);
            }
        }
    }

    protected boolean result(GraphHandler handler, PFlow pf, Relationship r, int level, boolean isOne) throws IOException {
    	PFlow pflow = new PFlow(pf);
		//System.out.println("check index "+r+" "+pf.getPathHash()[0]+" "+pf.getPFlowPath());
    	IndexHits<Relationship> i = Result.get(pflow.getPathHash(), r.getEndNode());
    	boolean found;
    	try {
	        found = iterate(handler, pflow, i, level, isOne);
    	} finally {
    		i.close();
    	}
        if (!found) {
            //UNDERSTAND: calculate current r!
            //System.out.println("READER Execute r = "+r);
            Iterator<Relationship[]>in = Evaluator._.execute(pflow, r);
            iterate(handler, pflow, in, level, isOne);
        }

        return found;

    }
    
    private Relationship getOp(Object obj) {
        if (obj.getClass().isArray()) {
            return ((Relationship[])obj)[0];
        } else {
        	return (Relationship)obj;
        }
    }

    protected boolean iterate(GraphHandler handler, PFlow pf, Iterator it, int level, boolean isOne) throws IOException {
        boolean found = false;
        boolean isFirst = isOne;
        Relationship i = null;
        int pos = 0;
        while (it.hasNext()) {
        	i = getOp(it.next());
            if (isFirst) {
                if (it.hasNext()) {
                    build(handler, pf, i, level, false, pos++, true);
                	i = getOp(it.next());
                    build(handler, pf, i, level, false, pos++, !it.hasNext());
                } else {
                    build(handler, pf, i, level, true, pos++, !it.hasNext());
                }
            } else {
                build(handler, pf, i, level, false, pos++, !it.hasNext());
            }
            isFirst = false;
            found = true;
        }
        return found;
    }
}
