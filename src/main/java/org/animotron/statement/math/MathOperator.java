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

import org.animotron.expression.JExpression;
import org.animotron.graph.AnimoGraph;
import org.animotron.graph.GraphOperation;
import org.animotron.graph.Properties;
import org.animotron.graph.index.Order;
import org.animotron.manipulator.OnQuestion;
import org.animotron.manipulator.PFlow;
import org.animotron.manipulator.QCAVector;
import org.animotron.statement.operator.AN;
import org.animotron.statement.operator.Evaluable;
import org.animotron.statement.operator.Prepare;
import org.animotron.statement.operator.Utils;
import org.animotron.statement.query.GET;
import org.jetlang.channels.Subscribable;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.index.IndexHits;

import java.io.IOException;

import javolution.util.FastList;

import static org.animotron.expression.JExpression.value;
import static org.animotron.graph.RelationshipTypes.TRI;

/**
 *
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 */
public abstract class MathOperator extends AbstractMathOperator implements Evaluable, Prepare{

	protected MathOperator(String name) { super(name); }

    @Override
    public Subscribable<PFlow> onCalcQuestion() {
        return question;
    }

    protected abstract Number execute (Number a, Number b);
    protected abstract Number execute (Number a);

    private final Number execute (QCAVector vector) throws IOException {
    	Number a = param(vector);
    	if (a == null) return null;
    	return execute(a);
    }

    private final Number execute (Number a, QCAVector vector) throws IOException {
    	Number b = param(vector);
    	if (b == null) return a;
    	return execute(a, b);
    }

    private OnQuestion question = new OnQuestion() {

        @Override
        public void onMessage(final PFlow pf) {
        	if (!Utils.results(pf)) {
	            IndexHits<Relationship> params = Order.context(pf.getOP().getStartNode());
	            try {
	                Number x = null;
	                for (Relationship param : params) {
	                	for (QCAVector r : Utils.getTheRelationships(pf, pf.getVector().question(param))) {
		                	if (x == null) {
		                		if (params.hasNext())
		                			x = param(r);
		                		else
		                			x = execute(r);
		                			
		                	} else {
		                		x = execute(x, r);
                            }
	                	}
	                }
                    Relationship r = new JExpression(value(x));
                	//Relationship op = pf.getOP().getStartNode().getSingleRelationship(AN._, INCOMING);
                    //XXX: fix context
                    pf.sendAnswer(r);
	            } catch (IOException e) {
	                pf.sendException(e);
	                return;
	            } finally {
	                params.close();
	            }
        	}
            pf.done();
        }

    };

	@Override
	public Subscribable<PFlow> onPrepareQuestion() {
		return prepare;
	}


    private OnQuestion prepare = new OnQuestion() {
    	@Override
    	public void onMessage(final PFlow pf) {
    		final FastList<Node> thes = FastList.newInstance();
    		IndexHits<Relationship> hits = Order.context(pf.getOPNode());
    		try {
    			for (Relationship r : hits) {
    				if (!r.isType(GET._)) {
    					pf.done();
    					return;
    				}
    				
    				for (QCAVector v : AN.getREFs(pf, new QCAVector(r))) {
    					thes.add(v.getClosest().getEndNode());
    				}
    				
    				if (thes.size() > 2) {
    					pf.done();
    					return;
    				}
    			}
    			
				if (thes.size() == 2) {

	    			AnimoGraph.execute(new GraphOperation<Void>() {
	
						@Override
						public Void execute() throws Exception {
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
    		pf.done();
    	}
    };
}
