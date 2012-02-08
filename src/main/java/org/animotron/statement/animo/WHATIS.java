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
import org.animotron.manipulator.OnQuestion;
import org.animotron.manipulator.PFlow;
import org.animotron.statement.operator.Reference;
import org.animotron.statement.operator.Utils;
import org.animotron.statement.query.AbstractQuery;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.index.IndexHits;

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
        	
			FastSet<Relationship> thes = FastSet.newInstance();
			try {
				Utils.getTHELikeBag(pf, pf.getVector(), thes);
				
				for (FastSet.Record rc = thes.head(), end = thes.tail(); (rc = rc.getNext()) != end;) {
					Relationship the = thes.valueOf(rc);

	    			IndexHits<Relationship> hits = Order.queryDown(the.getEndNode());
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
			} finally {
				FastSet.recycle(thes);
    		}
        }
    }
}
