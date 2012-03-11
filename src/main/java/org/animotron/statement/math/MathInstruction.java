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
package org.animotron.statement.math;

import javolution.util.FastList;
import org.animotron.graph.AnimoGraph;
import org.animotron.graph.GraphOperation;
import org.animotron.graph.Properties;
import org.animotron.graph.index.Order;
import org.animotron.io.Pipe;
import org.animotron.manipulator.OnQuestion;
import org.animotron.manipulator.PFlow;
import org.animotron.manipulator.QCAVector;
import org.animotron.statement.instruction.DetermInstruction;
import org.animotron.statement.operator.AN;
import org.animotron.statement.operator.Evaluable;
import org.animotron.statement.operator.Prepare;
import org.animotron.statement.operator.Utils;
import org.animotron.statement.query.GET;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.index.IndexHits;

import java.io.IOException;

import static org.animotron.graph.RelationshipTypes.TRI;

/**
 *
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 */
public abstract class MathInstruction extends DetermInstruction implements Evaluable, Prepare {

	protected MathInstruction(String name) { super(name); }

    protected abstract Relationship execute(final PFlow pf, AnimObject a, AnimObject b) throws IOException;

    protected abstract Relationship execute(final PFlow pf, AnimObject a) throws IOException;

    @Override
    public OnQuestion onCalcQuestion() {
        return new Calc();
    }
        		
	class Calc extends OnQuestion {
		
        @Override
        public void act(final PFlow pf) throws IOException {
        	if (!Utils.results(pf)) {
                AnimObject x = new AnimObject(MathInstruction.this, pf.getOP());
	                
                Relationship res = x.relax(pf);
                if (res != null)
                	pf.sendAnswer(res);
        	}
        }
    }

	@Override
	public OnQuestion onPrepareQuestion() {
		return new Prepare();
	}
	
	class Prepare extends OnQuestion {
		public boolean needAnswer() {
			return false;
		}

		@Override
    	public void act(final PFlow pf) throws Throwable {
    		final FastList<Node> thes = FastList.newInstance();
    		IndexHits<Relationship> hits = Order._.context(pf.getOPNode());
    		try {
    			for (Relationship r : hits) {
    				if (!r.isType(GET._)) {
    					return;
    				}
    				Pipe p = AN.getREFs(pf, new QCAVector(r));
    				QCAVector v;
    				while ((v = p.take()) != null) {
    					thes.add(v.getClosest().getEndNode());
    				}
    				if (thes.size() > 2) {
    					return;
    				}
    			}
				if (thes.size() == 2) {
	    			AnimoGraph.execute(new GraphOperation<Void>() {
						@Override
						public Void execute() throws Throwable {
							Relationship r = thes.get(0).createRelationshipTo(thes.get(1), TRI);
							Properties.TYPE.set(r, name());
							Properties.TO_NODE.set(r, pf.getOP().getStartNode().getId());
							return null;
						}
	    			});
				}
    		} finally {
    			hits.close();
    			FastList.recycle(thes);
    		}
    	}
    }
}
