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
import org.animotron.manipulator.Evaluator;
import org.animotron.manipulator.OnQuestion;
import org.animotron.manipulator.PFlow;
import org.animotron.manipulator.QCAVector;
import org.animotron.statement.Statement;
import org.animotron.statement.Statements;
import org.animotron.statement.operator.AN;
import org.animotron.statement.operator.Evaluable;
import org.animotron.statement.operator.Prepare;
import org.animotron.statement.operator.THE;
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
public abstract class MathOperator extends AbstractMathOperator implements Evaluable, Prepare {

	protected MathOperator(String name) { super(name); }

//    protected abstract AnimObject execute (AnimObject a, AnimObject b) throws IOException;
//    protected abstract AnimObject execute (AnimObject a);

    protected AnimObject execute(AnimObject a, AnimObject b) throws IOException {
    	return new AnimObject(this, a, b);
    }

    protected AnimObject execute(AnimObject a) throws IOException {
        return a;
    }

//    private final AnimObject execute (Relationship r) throws IOException {
//    	AnimObject a = param(r);
//    	if (a == null) return null;
//    	return execute(a);
//    }

    private final AnimObject execute (AnimObject a, Relationship r) throws IOException {
    	AnimObject b = param(r);
    	if (b == null) return a;
    	return execute(a, b);
    }

    @Override
    public OnQuestion onCalcQuestion() {
        return new Calc();
    }
        		
	class Calc extends OnQuestion {
		
        @Override
        public void act(final PFlow pf) throws IOException {
        	if (!Utils.results(pf)) {
	            IndexHits<Relationship> params = Order._.context(pf.getOP().getStartNode());
	            try {
	                AnimObject x = null;
	                for (Relationship param : params) {
	        			Statement s = Statements.relationshipType(param);
	        			System.out.println(s);
	        			if (s instanceof AN) {
                            try {
                				Object obj = THE._.reference(param);
                				if (obj != null && obj instanceof String ) {
                					s = Statements.name((String) obj);
                					if (s instanceof MathOperator) {
                						x = action(x, param);
                						continue;
                					}
                				} else {
                					//XXX: log
                					System.out.println("WARNING: REF but no name");
                					continue;
                				}
                            } catch (Throwable t){}							
	        			}
	        			if (s instanceof Evaluable) {
	        				if (s instanceof MathOperator) {
        						x = action(x, param);
        						continue;
	        				} else {
								Pipe p = Evaluator._.execute(
											pf.getController(), pf.getVector().question(param), null, false);
								
								QCAVector rr;
								while ((rr = p.take()) != null) {
									x = action(x, rr.getClosest());
								}
	        				}
	                	} else {
	                		throw new RuntimeException("Unknown operator.");
	                	}
	                }
                    //Relationship r = new JExpression(value(x));
                	//Relationship op = pf.getOP().getStartNode().getSingleRelationship(AN._, INCOMING);
                    //XXX: fix context
                    //pf.sendAnswer(r);
	                
	                pf.sendAnswer(x);
	            } finally {
	                params.close();
	            }
        	}
        }
        
        private AnimObject action(AnimObject x, Relationship r) throws IOException {
        	if (x == null)
    			return param(r);
        	
        	else
        		return execute(x, r);
        }
    }

//	class Calc extends OnQuestion {
//	
//        @Override
//        public void act(final PFlow pf) throws IOException {
//        	if (!Utils.results(pf)) {
//	            IndexHits<Relationship> params = Order._.context(pf.getOP().getStartNode());
//	            try {
//	                Number x = null;
//	                for (Relationship param : params) {
//	                	Pipe pipe = Utils.getTheRelationships(pf, pf.getVector().question(param));
//	                	QCAVector r;
//	                	while ((r = pipe.take()) != null) {
//		                	if (x == null) {
//		                		if (params.hasNext())
//		                			x = param(r);
//		                		else
//		                			x = execute(r);
//		                			
//		                	} else {
//		                		x = execute(x, r);
//                            }
//	                	}
//	                }
//                    Relationship r = new JExpression(value(x));
//                	//Relationship op = pf.getOP().getStartNode().getSingleRelationship(AN._, INCOMING);
//                    //XXX: fix context
//                    pf.sendAnswer(r);
//	            } finally {
//	                params.close();
//	            }
//        	}
//        }
//    }

	@Override
	public OnQuestion onPrepareQuestion() {
		return new Prepare();
	}
	
	class Prepare extends OnQuestion {
		public boolean needAnswer() {
			return false;
		}

		@Override
    	public void act(final PFlow pf) {
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
