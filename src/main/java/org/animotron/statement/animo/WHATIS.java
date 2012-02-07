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
import org.animotron.statement.instruction.Instruction;
import org.animotron.statement.operator.AN;
import org.animotron.statement.operator.Evaluable;
import org.animotron.statement.operator.REF;
import org.animotron.statement.operator.Utils;
import org.animotron.statement.query.AbstractQuery;
import org.neo4j.graphdb.Direction;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.index.IndexHits;

/**
 * Return IS relations of object.
 * 
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class WHATIS extends Instruction implements Evaluable {
	
	public static final WHATIS _ = new WHATIS();
	
	private WHATIS() { super("what-is", "whatis"); }

	@Override
	public OnQuestion onCalcQuestion() {
		return new Calc();
	}

    class Calc extends OnQuestion {
        @Override
        public void act(final PFlow pf) {
        	
        	Pipe pipe = Evaluator._.execute(pf.getController(), pf.getVector(), pf.getOP().getStartNode());
    		QCAVector v;
    		while ((v = pipe.take()) != null) {
            	IndexHits<Relationship> hits = Order.queryDown(v.getClosest().getEndNode());
            	try {
                	for (Relationship rr : hits) {
	        			if (!isLeaf(rr.getStartNode())) {
	        				pf.sendAnswer( rr );
	        			}
                	}
            	} finally {
            		hits.close();
            	}
    		}
        }
    }
    
    protected boolean isLeaf(Node node) {
		for (Relationship r : node.getRelationships(Direction.INCOMING, REF._))
			if (r.getStartNode().hasRelationship(Direction.INCOMING, AN._))
				return false;
		
		return true;
	};

}
