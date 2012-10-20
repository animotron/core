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
package org.animotron.statement.animo;

import javolution.util.FastSet;
import org.animotron.graph.index.Order;
import org.animotron.io.Pipe;
import org.animotron.manipulator.Evaluator;
import org.animotron.manipulator.OnQuestion;
import org.animotron.manipulator.PFlow;
import org.animotron.manipulator.QCAVector;
import org.animotron.statement.operator.*;
import org.animotron.statement.query.AbstractQuery;
import org.neo4j.graphdb.Direction;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.index.IndexHits;

import java.io.IOException;

/**
 * Return IS relations of object.
 * 
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class WHATIS extends AbstractQuery implements Reference {
	
	public static final WHATIS _ = new WHATIS();
	
	private WHATIS() { super("what-is", "whatis"); }

	@Override
	public OnQuestion onCalcQuestion() {
		return new Calc();
	}

    class Calc extends OnQuestion {
        @Override
        public void act(final PFlow pf) {
        	
//        	System.out.println("WHATIS "+pf.getVector());
        	
			FastSet<QCAVector> thes = FastSet.newInstance();
			try {
				Utils.getTHEBag(pf, pf.getVector(), thes);
				
				for (FastSet.Record rc = thes.head(), end = thes.tail(); (rc = rc.getNext()) != end;) {
					QCAVector vector = thes.valueOf(rc);
					Relationship the = vector.getClosest();
					
					if (the.isType(REF._) || the.isType(DEF._)) {
						downIS(pf, the.getEndNode());
						
					} else {
			    		//discover down IS topology
						discoverDownIS(pf, vector, the);
					}
					upIS(pf, vector, the);
				}
			} finally {
				FastSet.recycle(thes);
    		}
        }
        
        private void downIS(PFlow pf, Node node) {
			IndexHits<Relationship> hits = Order._.queryDown(node);
        	try {
            	for (Relationship rr : hits) {
        			if (!Utils.haveContext(rr.getEndNode())) {
        				pf.sendAnswer( rr );
        			}
            	}
        	} finally {
        		hits.close();
        	}
        }

        private void upIS(PFlow pf, QCAVector vector, Relationship the) {
    		//discover up IS topology
    		for (Relationship r : the.getStartNode().getRelationships(AN._, Direction.INCOMING)) {
        		for (Relationship rr : r.getStartNode().getRelationships(AN._, Direction.INCOMING)) {
        			discoverDownIS(pf, vector, rr);
        		}
    		}
        }

        private void discoverDownIS(PFlow pf, QCAVector vector, Relationship the) {
			Pipe pipe;
			try {
				pipe = Evaluator._.execute(pf.getController(), vector.question2(the));
			} catch (IOException e) {
				pf.sendException(e);
				return;
			}
    		QCAVector v;
    		while ((v = pipe.take()) != null) {
				downIS(pf, v.getClosest().getEndNode());
    		}
        }
    }
}
