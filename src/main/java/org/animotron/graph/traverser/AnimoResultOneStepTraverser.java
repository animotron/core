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

import org.animotron.graph.handler.AnimoGraphHandler;
import org.animotron.graph.handler.GraphHandler;
import org.animotron.manipulator.Evaluator;
import org.animotron.manipulator.PFlow;
import org.animotron.manipulator.QCAVector;
import org.animotron.statement.Statement;
import org.animotron.statement.Statements;
import org.animotron.statement.operator.AN;
import org.animotron.statement.operator.Evaluable;
import org.animotron.statement.operator.REF;
import org.animotron.statement.operator.Shift;
import org.animotron.statement.relation.USE;
import org.neo4j.graphdb.Relationship;

import java.io.IOException;
import java.util.Iterator;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovskyd@gmail.com">Evgeny Gazdovsky</a>
 * 
 */
public class AnimoResultOneStepTraverser extends ResultTraverser {
	
    public AnimoResultOneStepTraverser() {}

    @Override
    protected void process(GraphHandler handler, PFlow pf, Statement s, Statement parent, QCAVector rr, int level, boolean isOne, int pos, boolean isLast) throws IOException {
        if (s != null) {
        	Statement qS = Statements.relationshipType(rr.getQuestion());
        	if (((qS instanceof Shift && rr.getUnrelaxedAnswer() == null)
        			|| (s instanceof Evaluable && !(qS instanceof Shift)))
    			&& !handler.isStepMade() ) {

            	GraphHandler gh = new AnimoGraphHandler(handler);
                result(gh, pf, rr, level, isOne);
                
			//workaround IS and USE
			} else if (s instanceof USE) {
				Relationship r = rr.getClosest();
				
				handler.start(s, parent, r, level++, isOne, pos, isLast);
				handler.end(s, parent, r, --level, isOne, pos, isLast);
            } else {
				Relationship r = rr.getClosest();

				handler.start(s, parent, r, level++, isOne, pos, isLast);
                if (!(s instanceof REF && !(qS instanceof AN))) {
                    node = r.getEndNode();
                    iterate(handler, pf, s, new It(node), level);
                }
                handler.end(s, parent, r, --level, isOne, pos, isLast);
            }
        }
    }
    
    protected boolean result(GraphHandler handler, PFlow pf, QCAVector rr, int level, boolean isOne) throws IOException {
    	Relationship r = rr.getClosest();
    	
    	PFlow pflow = new PFlow(pf);

    	//System.out.println("check index "+r+" "+pf.getPathHash()[0]+" "+pf.getPFlowPath());
//    	IndexHits<QCAVector> i = Result.get(pflow.getPathHash(), r);
//    	boolean found;
//    	try {
//	        found = iterate(handler, pflow, i, level, isOne);
//    	} finally {
//    		i.close();
//    	}
//        if (!found) {
            Iterator<QCAVector>in = Evaluator._.execute(pflow, new QCAVector(r, rr), null, false);
            iterate(handler, pflow, null, in, level, isOne);
//        }

        return true;
    }

}
