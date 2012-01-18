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
package org.animotron.manipulator;

import org.animotron.Executor;
import org.animotron.exception.AnimoException;
import org.animotron.graph.index.Order;
import org.animotron.io.Pipe;
import org.animotron.marker.Marker;
import org.animotron.statement.Statement;
import org.animotron.statement.Statements;
import org.animotron.statement.operator.Evaluable;
import org.animotron.statement.operator.REF;
import org.animotron.statement.operator.Shift;
import org.animotron.statement.operator.THE;
import org.jetlang.channels.Subscribable;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.index.IndexHits;

import java.io.IOException;
import java.util.Iterator;

import javolution.util.FastTable;

import static org.animotron.graph.RelationshipTypes.*;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public abstract class Manipulator {

//	protected abstract void prepare(Relationship op, PFlow ch);

	public Marker marker(){
		return null;
	}
	
	public Pipe execute(final QCAVector vector, final Node node) {
        final Pipe pipe = Pipe.newInstance();
		
        Executor.execute(new Runnable() {
			@Override
			public void run() {
		        sendQuestion(pipe, vector, node);
			}
		});
        
        return pipe;
	}

	public final Pipe execute(Relationship op) throws IOException {
		return execute(new QCAVector(op), null, true);
	}
	
	public final Pipe execute(QCAVector vector) throws IOException {
		return execute(vector, null, true);
	}

	public final Pipe execute(final QCAVector vector, OnQuestion sub, final boolean fullEval) throws IOException {
        final Pipe pipe = Pipe.newInstance();

        final Relationship op = vector.getClosest();
        if (sub == null) {
			sub = onQuestion(op);
        }
		
		if (sub == null) {
			Executor.execute(new Runnable() {
				@Override
				public void run() {
					if (op instanceof Relationship) {
						vector.setAnswer(op);
						try {
							pipe.write( vector );
						} catch (IOException e) {
							//XXX: log
							e.printStackTrace();
						}
					} else
						System.out.println("UNHANDLED op "+op);
					pipe.close();
				}
			});
			return pipe;
		}
		
		final PFlow pf = new PFlow(vector);
		try {
			pf.cyclingDetection();
		} catch (AnimoException e) {
			e.printStackTrace();
			throw new IOException(e);
		}
		pf.questionChannel().subscribe(sub.getFiber(), sub);
		
        //answers transfer to output
		OnContext onAnswer = new OnContext() {
            public void onMessage(QCAVector context) {
            	super.onMessage(context);
            	
            	//System.out.println("get answer "+context);
            	//out.debug();
            	try {
            		if (context == null) {

            			pf.countDown(pipe);

            		} else if (context.getAnswer() != null) {
//            			int addedContexts = 0;
                        Statement s = null;
                        
                        Relationship msg = context.getAnswer();

        				if (context.getUnrelaxedAnswer().isType(RESULT)) {
        					Statement qS = Statements.relationshipType(context.getQuestion());

        					if (qS instanceof Shift) {
                				pipe.write(context);
                				return;
        					
        					} else if (!fullEval) {
                				pipe.write(context);
                				return;
                			}
                			
                            try {
                                s = Statements.name((String) THE._.reference(msg));
                            } catch (Exception e){}
        				}

            			if (msg.isType(REF._)) {
                            s = Statements.name((String) THE._.reference(msg));
                        }
            			

                        if (s != null && s instanceof Evaluable) {
                        	Pipe in = execute(context, ((Evaluable) s).onCalcQuestion(), fullEval);
                        	QCAVector v;
                            while ((v = in.take()) != null) {
                            	pipe.write(v);
                            }
                        } else if (s == null){
                            s = Statements.relationshipType(msg);
                            Statement qS = Statements.relationshipType(context.getQuestion());
                            if (s instanceof Evaluable && !(qS instanceof Shift)) {
                                Pipe in = Evaluator._.execute(context);
                                QCAVector v;
                                while ((v = in.take()) != null) {
                                    pipe.write(v);
                                }
                            } else {
                                pipe.write(context);
                            }
                        } else {
                        	pipe.write(context);
                        }
                    } else {
                        //what to do if msg is null?
                    	//ignore -- XXX: log warning
                    }
				} catch (IOException e) {
					pf.sendException(e);
				}
            }
        };
		
        pf.answerChannel().subscribe(onAnswer.getQueue(), onAnswer); 

        //send question to evaluation
        pf.questionChannel().publish(pf);
        //System.out.println(Thread.currentThread()+" "+pf.getVector());
		
		//XXX: what to do with this?
        //reset.await(5, TimeUnit.SECONDS);
		
		return pipe;
	}
	
	public OnQuestion onQuestion(final Relationship op) {
		return null;//new OnQuestion();
	}

	//XXX: private
	public static void sendQuestion(final Pipe pipe, final QCAVector vector, final Node node) {
		OnContext onAnswer = new OnContext(Executor.getFiber()) {
            public void onMessage(QCAVector context) {
            	super.onMessage(context);
            	
            	try {
            		if (context != null)
            			pipe.write(context);

	        		if (cd != null && cd.getCount() == 0)
	        			pipe.close();
	        		
            	} catch (IOException e) {
            		//XXX: what to do?
					e.printStackTrace();
				}
            }
        };

		sendQuestion(onAnswer, vector, node);
	}
	
	public static void sendQuestion(final OnContext onAnswer, final QCAVector vector, final Node node) {
		
		//System.out.println("sendQuestion");
		
		//final CountDownLatch cd;
		
		FastTable<PFlow> list = FastTable.newInstance();
		try {
			PFlow nextPF = null;

			IndexHits<Relationship> q = Order.context(node);
			try {
				Iterator<Relationship> it = q.iterator();
				while (it.hasNext()) {
					Relationship r = it.next();
					
					Subscribable<PFlow> onQuestion = Evaluator._.onQuestion(r);
					
					if (onQuestion != null) {
						nextPF = new PFlow(vector.question2(r));
						nextPF.questionChannel().subscribe(onQuestion);
						
						list.add(nextPF);
						
					} else {
						onAnswer.onMessage(vector.answered(r));
					}
				}
			} finally {
				q.close();
			}
			
			if (list.isEmpty()) {
				onAnswer.setCountDown(1);
				onAnswer.onMessage(null);				
				return;
			}
	
			onAnswer.setCountDown(list.size());
			//cd = new CountDownLatch(list.size());
			
			for (int i = 0, n = list.size(); i < n; i++) {
				nextPF = list.get(i);
				
				nextPF.answerChannel().subscribe(onAnswer);
				
				nextPF.questionChannel().publish(nextPF);
			}

		} finally {
			FastTable.recycle(list);
		}
	}
}