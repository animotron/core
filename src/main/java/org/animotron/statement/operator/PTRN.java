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
package org.animotron.statement.operator;

import javolution.util.FastSet;
import org.animotron.graph.index.Order;
import org.animotron.io.Pipe;
import org.animotron.manipulator.Evaluator;
import org.animotron.manipulator.OnQuestion;
import org.animotron.manipulator.PFlow;
import org.animotron.manipulator.QCAVector;
import org.animotron.statement.Statement;
import org.animotron.statement.Statements;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.index.IndexHits;

import java.io.IOException;


/**
 * Operator 'ptrn'.
 * 
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 */
public class PTRN extends Operator implements Evaluable {
	
	public static final PTRN _ = new PTRN();
	
	private PTRN() { super("ptrn"); }

	@Override
	public OnQuestion onCalcQuestion() {
		return new Calc();
	}
	
    class Calc extends OnQuestion {
        @Override
        public void act(final PFlow pf) throws Throwable {
        	System.out.println("ptrn");

        	FastSet<Relationship> thes = FastSet.newInstance();
			try {
				Utils.getTHELikeBag(pf, pf.getVector(), thes);
				
				for (Relationship the : thes) {
					QCAVector answered = pf.getVector().answered(the);
					
					if (!filtering(pf, the, answered)) {
						evalOther(pf, the, answered);
					}
				}
			} finally {
				FastSet.recycle(thes);
			}
        }

        private boolean filtering(final PFlow pf, final Relationship ref, final QCAVector answered) throws IOException, InterruptedException {
        	IndexHits<Relationship> hits = Order._.context(pf.getOPNode());
        	try {
    	        for (Relationship r : hits) {
    	            Statement st = Statements.relationshipType(r);
    	            if (st instanceof Predicate) {
	                    if (((Predicate) st).filter(pf, r, ref)) {
	                    	Pipe pipe = Evaluator._.execute(pf.getController(), answered, r.getEndNode());
	                		QCAVector v;
	                		while ((v = pipe.take()) != null) {
	                			pf.sendAnswer(v);
	                		}
	                        return true;
	                    }
    	            }
    	        }
        	} finally {
        		hits.close();
        	}
            return false;
        }
        
        private void evalOther(final PFlow pf, final Relationship ref, final QCAVector answered) throws IOException {
        	IndexHits<Relationship> hits = Order._.context(pf.getOPNode());
        	try {
    	        for (Relationship r : hits) {
    	            Statement st = Statements.relationshipType(r);
    	            if (!(st instanceof Predicate)) {
	            		Pipe pipe = Evaluator._.execute(pf.getController(), answered.question(r));
                		QCAVector v;
                		while ((v = pipe.take()) != null) {
                			pf.sendAnswer(v);
                		}
    	            }
    	        }
        	} finally {
        		hits.close();
        	}
        }
    }
}
