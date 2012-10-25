/*
 *  Copyright (C) 2011-2012 The Animo Project
 *  http://animotron.org
 *  	
 *  This file is part of Animotron.
 *  
 *  Animotron is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU Affero General Public License as 
 *  published by the Free Software Foundation, either version 3 of 
 *  the License, or (at your option) any later version.
 *  
 *  Animotron is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Affero General Public License for more details.
 *  
 *  You should have received a copy of 
 *  the GNU Affero General Public License along with Animotron.  
 *  If not, see <http://www.gnu.org/licenses/>.
 */
package org.animotron.graph.traverser;

import org.animotron.graph.handler.GraphHandler;
import org.animotron.io.PipeIterator;
import org.animotron.manipulator.Evaluator;
import org.animotron.manipulator.QCAVector;
import org.animotron.statement.Statement;
import org.animotron.statement.Statements;
import org.animotron.statement.operator.*;
import org.animotron.statement.value.AbstractValue;
import org.neo4j.graphdb.Relationship;

import java.io.IOException;
import java.util.Iterator;

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
        build(handler, null, r, 0, true, 0, true, true);
        handler.endGraph();
    }

    @Override
    protected void build(GraphHandler handler, Statement parent, QCAVector rr, int level, boolean isOne, int pos, boolean isLast, boolean evaluable) throws IOException {

    	Relationship r = rr.getClosest();
    	
		Statement s = Statements.relationshipType(r);
	        
        process(handler, s, parent, rr, level, isOne, pos, isLast, evaluable);
    }
    
    protected void process(GraphHandler handler, Statement s, Statement parent, QCAVector rr, int level, boolean isOne, int pos, boolean isLast, boolean evaluable) throws IOException {
    	Statement qS = Statements.relationshipType(rr.getQuestion());
    	if (qS instanceof Definition && rr.hasAnswer()) {
        	Relationship r = rr.getClosest();

			handler.start(qS, null, rr.getQuestion(), level++, isOne, pos, isLast);
            iterate(handler, rr, s, new It(r.getEndNode()), level, evaluable);
            handler.end(qS, null, rr.getQuestion(), --level, isOne, pos, isLast);

        } else if (s != null) {
			//avoid cycling
			if (rr.hasAnswer() && rr.getAnswer().equals(rr.getQuestion()))
				evaluable = false;

        	if (evaluable && s instanceof Evaluable) { 

        		result(handler, rr, level, isOne, pos, isLast);
			
        	} else {
				Relationship r = rr.getClosest();
				
                if (s instanceof AbstractValue)
                    handler.start(s, parent, r, level++, isOne, pos, isLast);
                
                if (!(s instanceof REF  && !(qS instanceof AN))) {
                    node = AREV._.actualEndNode(r);
	                iterate(handler, rr, s, new It(node), level, evaluable);
                }
                
                if (s instanceof AbstractValue)
                    handler.end(s, parent, r, --level, isOne, pos, isLast);
            }
        }
    }

    protected void result(GraphHandler handler, QCAVector rr, int level, boolean isOne, int pos, boolean isLast) throws IOException {
    	Relationship r = rr.getClosest();
    	
		Iterator<QCAVector> in = 
        		new PipeIterator( 
        				Evaluator._.execute(handler.getController(), rr.question(r)) );
        
        iterate(handler, null, rr, in, level, isOne, pos, isLast, true);
    }
    
    protected boolean iterate(GraphHandler handler, Statement parent, QCAVector rr, Iterator<QCAVector> it, int level, boolean isOne, int pos, boolean isLast, boolean evaluable) throws IOException {
        boolean found = false;
        boolean isFirst = isOne;

//        QCAVector prev = null;
        
        QCAVector i;
        while (it.hasNext()) {
        	i = it.next();
        	rr.addAnswer(i);
//        	i.setPrecedingSibling(prev);
//        	prev = i;
            if (isFirst) {
                if (it.hasNext()) {
                    build(handler, parent, i, level, false, pos++, isLast, evaluable);
                	i = it.next();
//                	i.setPrecedingSibling(prev);
//                	prev = i;
                    build(handler, parent, i, level, false, pos++, isLast && !it.hasNext(), evaluable);
                } else {
                    build(handler, parent, i, level, isOne, pos++, isLast && !it.hasNext(), evaluable);
                }
            } else {
                build(handler, parent, i, level, false, pos++, isLast && !it.hasNext(), evaluable);
            }
            isFirst = false;
            found = true;
        }
        return found;
    }
}
