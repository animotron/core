/*
 *  Copyright (C) 2011 The Animo Project
 *  http://animotron.org
 *
 *  This program is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public License
 *  as published by the Free Software Foundation; either version 3
 *  of the License, or (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */
package org.animotron.manipulator;

import org.animotron.Executor;
import org.animotron.exception.AnimoException;
import org.animotron.graph.index.Order;
import org.animotron.io.PipedInput;
import org.animotron.io.PipedOutput;
import org.animotron.marker.Marker;
import org.animotron.statement.Statement;
import org.animotron.statement.Statements;
import org.animotron.statement.operator.Evaluable;
import org.animotron.statement.operator.Shift;
import org.animotron.statement.operator.THE;
import org.jetlang.channels.Subscribable;
import org.jetlang.core.DisposingExecutor;
import org.jetlang.fibers.Fiber;
import org.jetlang.fibers.ThreadFiber;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.index.IndexHits;

import java.io.IOException;
import java.util.Iterator;
import java.util.concurrent.CountDownLatch;

import javolution.util.FastList;

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
	
	public PipedInput<QCAVector> execute(QCAVector vector, Node node) throws IOException {
        final PipedOutput<QCAVector> out = new PipedOutput<QCAVector>();
        PipedInput<QCAVector> in = out.getInputStream();
		
        sendQuestion(out, vector, node);
        
        return in;
	}

	public final PipedInput<QCAVector> execute(Relationship op) throws IOException {
		return execute(new QCAVector(op), null, true);
	}
	
	public final PipedInput<QCAVector> execute(QCAVector vector) throws IOException {
		return execute(vector, null, true);
	}

	public final PipedInput<QCAVector> execute(QCAVector vector, Subscribable<PFlow> sub, final boolean fullEval) throws IOException {
        final PipedOutput<QCAVector> out = new PipedOutput<QCAVector>();
        PipedInput<QCAVector> in = out.getInputStream();

        Relationship op = vector.getClosest();
        if (sub == null) {
			sub = onQuestion(op);
        }
		
		if (sub == null) {
			if (op instanceof Relationship) {
				vector.setAnswer(op);
				out.write( vector );
			} else
				System.out.println("UNHANDLED op "+op);
			out.close();
			return in;
		}
		
		final PFlow pf = new PFlow(vector);
		try {
			pf.cyclingDetection();
		} catch (AnimoException e) {
			e.printStackTrace();
			throw new IOException(e);
		}
		pf.questionChannel().subscribe(Executor.getFiber(), sub);

		
        //answers transfer to output
        Subscribable<QCAVector> onAnswer = new OnContext(Executor.getFiber()) {
            public void onMessage(QCAVector context) {
            	super.onMessage(context);
            	
            	//System.out.println("get answer "+context);
            	try {
            		if (context == null) {

            			pf.countDown(out);

            		} else if (context.getAnswer() != null) {
//            			int addedContexts = 0;
                        Statement s = null;
                        
                        Relationship msg = context.getAnswer();

        				if (context.getUnrelaxedAnswer().isType(RESULT)) {
        					Statement qS = Statements.relationshipType(context.getQuestion());

        					if (qS instanceof Shift) {
                				out.write(context);
                				return;
        					
        					} else if (!fullEval) {
                				out.write(context);
                				return;
                			}
                			
                            try {
                                s = Statements.name((String) THE._.reference(msg));
                            } catch (Exception e){}
        				}

            			if (msg.isType(org.animotron.statement.operator.REF._) || msg.isType(REF)) {
                            s = Statements.name((String) THE._.reference(msg));
                        }
            			

                        if (s != null && s instanceof Evaluable) {
                        	PipedInput<QCAVector> in = execute(context, ((Evaluable) s).onCalcQuestion(), fullEval);
                            for (QCAVector v : in) {
                            	out.write(v);
                            }
                        } else if (s == null){
                            s = Statements.relationshipType(msg);
                            Statement qS = Statements.relationshipType(context.getQuestion());
                            if (s instanceof Evaluable && !(qS instanceof Shift)) {
                                PipedInput<QCAVector> in = Evaluator._.execute(context);
                                for (QCAVector v : in) {
                                    out.write(v);
                                }
                            } else {
                                out.write(context);
                            }
                        } else {
                            out.write(context);
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
		
		//XXX: what to do with this?
        //reset.await(5, TimeUnit.SECONDS);
		
		return in;
	}
	
	public Subscribable<PFlow> onQuestion(final Relationship op) {
		return new OnQuestion();
	}

	public static void sendQuestion(final PipedOutput<QCAVector> out, final QCAVector vector, final Node node) throws IOException {
		OnContext onAnswer = new OnContext(Executor.getFiber()) {
            public void onMessage(QCAVector context) {
            	super.onMessage(context);
            	
            	try {
            		if (context != null)
            			out.write(context);

	        		if (cd != null && cd.getCount() == 0)
	        			out.close();
            	} catch (IOException e) {
					e.printStackTrace();
				}

            }
        };

		sendQuestion(onAnswer, vector, node);
		
	}
	
	public static void sendQuestion(final OnContext onAnswer, final QCAVector vector, final Node node) {
		
		//final CountDownLatch cd;
		
		FastList<PFlow> list = FastList.newInstance();
		try {
			IndexHits<Relationship> q = Order.context(node);
			try {
				Iterator<Relationship> it = q.iterator();
				while (it.hasNext()) {
					Relationship r = it.next();
					
					Subscribable<PFlow> onQuestion = Evaluator._.onQuestion(r);
					
					if (onQuestion != null) {
						PFlow nextPF = new PFlow(vector.question2(r));
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
			
			for (PFlow nextPF : list) {
				nextPF.answerChannel().subscribe(onAnswer);
				
				nextPF.questionChannel().publish(nextPF);
			}

		} finally {
			FastList.recycle(list);
		}
	}
}
