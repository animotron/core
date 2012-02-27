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
package org.animotron.statement.language;

import javolution.util.FastSet;
import org.animotron.Executor;
import org.animotron.expression.JExpression;
import org.animotron.graph.AnimoGraph;
import org.animotron.graph.GraphOperation;
import org.animotron.graph.index.AbstractRelationshipIndex;
import org.animotron.graph.index.Order;
import org.animotron.io.Pipe;
import org.animotron.manipulator.*;
import org.animotron.statement.instruction.DetermInstruction;
import org.animotron.statement.operator.Prepare;
import org.animotron.statement.operator.Utils;
import org.animotron.statement.query.GET;
import org.animotron.statement.string.STRING;
import org.animotron.statement.value.VALUE;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.index.IndexHits;
import org.neo4j.graphdb.index.IndexManager;

import static org.animotron.expression.JExpression.value;

/**
 * 'WORD' instruction.
 *
 *  @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 */
public class WORD extends DetermInstruction implements Prepare {

	public static final WORD _ = new WORD();
	
	private static final String NAME = "word";

	private WORD() {super(NAME);}

	private AbstractRelationshipIndex words = new AbstractRelationshipIndex(NAME) {
        @Override
        public void init(IndexManager index) {
            init(index.forRelationships(name));
        }
    };
	
	public void init(IndexManager index) {
		words.init(index);
	}


	@Override
	public OnQuestion onCalcQuestion() {
		return new Calc();
	}
	
	class Calc extends OnQuestion {
        @Override
        public void act(final PFlow pf) {
        	
        	final FastSet<Node> thes = FastSet.newInstance();
        	final FastSet<QCAVector> REFs = FastSet.newInstance();
			try {
				Utils.getTHEbag(pf, pf.getVector(), thes);
				
				Pipe in = Evaluator._.execute(pf.getController(), pf.getVector(), pf.getOP().getStartNode());
        		QCAVector v;
        		while ((v = in.take()) != null) {
        			REFs.add(new QCAVector(v.getQuestion(), v.getAnswer()));
        		}

        		final PFlow pflow = new PFlow(pf.getController(), new QCAVector(pf.getVector().getQuestion()));
        		
        		final Pipe pipe = Pipe.newInstance();

        		OnContext onContext = new OnContext() {
        			@Override
        			public void onMessage(QCAVector vector) {
        				super.onMessage(vector, pipe);
        			}
        		};
        		onContext.setCountDown(1);
        		pflow.answerChannel().subscribe(onContext);

        		Executor.execute(new Runnable() {
        			@Override
        			public void run() {
        	        	final FastSet<Relationship> visitedREFs = FastSet.newInstance();
        				try {
        					GET._.get(pflow, REFs, thes, visitedREFs, true);
        				} finally {
        					FastSet.recycle(visitedREFs);
        					pflow.done();
        				}
        			}
        		});
        		
        		StringBuilder sb = new StringBuilder(); 
        		
        		while ((v = pipe.take()) != null) {
        			STRING._.eval(sb, pf, v.getClosest().getEndNode());
        		}
        		
                if (sb.length() > 0) {

    	            Relationship r;
    				try {
    					r = new JExpression(
    					    value(
                                sb.toString()
                            )
    					);
    				} catch (Throwable t) {
    					pf.sendException(t);
    					return;
    				}
    				answered(pf, r);
                }
			} finally {
				FastSet.recycle(thes);
				FastSet.recycle(REFs);
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
    	public void act(final PFlow pf) {
			AnimoGraph.execute(new GraphOperation<Void>() {

				@Override
				public Void execute() throws Throwable {
					IndexHits<Relationship> hits = Order._.queryDown(pf.getOPNode());
					try {
						for (Relationship r : hits) {
							if (r.isType(VALUE._)) {
								words.add(r, VALUE._.reference(r));
							}
						}
					} catch (Throwable t) {
						t.printStackTrace();
					} finally {
						hits.close();
					}
					return null;
				}
			});
		}
	}

	public IndexHits<Relationship> search(String word) {
		//System.out.println(words.get(NAME, word).getSingle());
		return words.query(word);
	}
}