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

import org.animotron.graph.handler.AnimoGraphHandler;
import org.animotron.graph.handler.GraphHandler;
import org.animotron.io.PipeIterator;
import org.animotron.manipulator.Evaluator;
import org.animotron.manipulator.QCAVector;
import org.animotron.statement.Statement;
import org.animotron.statement.Statements;
import org.animotron.statement.operator.*;
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
    protected void process(GraphHandler handler, Statement s, Statement parent, QCAVector rr, int level, boolean isOne, int pos, boolean isLast, boolean evaluable, Relationship def) throws IOException {
    	Statement qS = Statements.relationshipType(rr.getQuestion());
    	if (qS instanceof Definition && rr.hasAnswer()) {
        	Relationship r = rr.getClosest();
			handler.start(qS, null, rr.getQuestion(), level++, isOne, pos, isLast);
            iterate(handler, rr, s, r, level, evaluable, def);
            handler.end(qS, null, rr.getQuestion(), --level, isOne, pos, isLast);
        } else if (s != null) {
			//avoid cycling
			if (rr.hasAnswer() && rr.getAnswer().equals(rr.getQuestion()))
				evaluable = false;
			if (evaluable && s instanceof Evaluable  && !handler.isStepMade() ) {
            	GraphHandler gh = new AnimoGraphHandler(handler);
                result(gh, rr, level, isOne, pos, isLast, def);
            } else {
				Relationship r = rr.getClosest();
				handler.start(s, parent, r, level++, isOne, pos, isLast);
                if (!(s instanceof REF && !(qS instanceof Reference))) {
                    iterate(s, handler, parent, rr, r, level, pos, evaluable,def);
                }
                handler.end(s, parent, r, --level, isOne, pos, isLast);
            }
        }
    }
    
    protected void result(GraphHandler handler, QCAVector rr, int level, boolean isOne, int pos, boolean isLast, Relationship def) throws IOException {
    	Relationship r = rr.getClosest();
    	
        Iterator<QCAVector> in = 
        		new PipeIterator( 
        				Evaluator._.execute(handler.getController(), rr.question(r), null, false) );
        
        iterate(handler, null, rr, in, level, isOne, pos, isLast, true, null);
    }
}