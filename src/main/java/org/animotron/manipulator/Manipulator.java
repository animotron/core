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
import org.animotron.inmemory.InMemoryRelationship;
import org.animotron.io.PipedInput;
import org.animotron.io.PipedOutput;
import org.animotron.marker.Marker;
import org.animotron.statement.Statement;
import org.animotron.statement.Statements;
import org.animotron.statement.operator.Evaluable;
import org.animotron.statement.operator.Query;
import org.animotron.statement.operator.THE;
import org.jetlang.channels.Subscribable;
import org.jetlang.core.DisposingExecutor;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;

import java.io.IOException;

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
	
	public final PipedInput<QCAVector> execute(Relationship op) throws IOException {
		return execute(new PFlow(this), op);
	}

	public final PipedInput<QCAVector> execute(final PFlow pf, Node op) throws IOException {
		Relationship r = new InMemoryRelationship(null, op, FAKE);
		return execute(pf, r);
	}

	public final PipedInput<QCAVector> execute(PFlow pf, Relationship op) throws IOException {
		return execute(pf, new QCAVector(op), null, true);
	}
	
	public final PipedInput<QCAVector> execute(PFlow pf, QCAVector vector) throws IOException {
		return execute(pf, vector, null, true);
	}

	public final PipedInput<QCAVector> execute(PFlow pflow, QCAVector vector, Subscribable<PFlow> sub, final boolean fullEval) throws IOException {
        final PipedOutput<QCAVector> out = new PipedOutput<QCAVector>();
        PipedInput<QCAVector> in = out.getInputStream();

        Relationship op = vector.getClosest();
        if (sub == null) {
			if (vector.getClosest().isType(FAKE)) {
				sub = onQuestion(null);
			} else {
				sub = onQuestion(op);
			}
        }
		
		if (sub == null) {
			if (op instanceof Relationship) {
				out.write( new QCAVector((Relationship)op, (Relationship)op) ); //XXX: is it correct???
			} else
				System.out.println("UNHANDLED op "+op);
			out.close();
			return in;
		}
		
		final PFlow pf;
		if (op instanceof Node) {
			pf = new PFlow(pflow, (Node)op);
		} else {
			try {
				pf = new PFlow(pflow, (Relationship)op);
			} catch (AnimoException e) {
				e.printStackTrace();
				throw new IOException(e);
			}
		}
		pf.question.subscribe(sub);

		
        //answers transfer to output
        Subscribable<QCAVector> onAnswer = new Subscribable<QCAVector>() {
            public void onMessage(QCAVector context) {
            	System.out.println("get answer "+context);
            	try {
            		if (context == null) {

            			pf.countDown(out);

            		} else if (context.getAnswer() != null) {
//            			int addedContexts = 0;
                        Statement s = null;
                        
                        Relationship msg = context.getAnswer();

        				if (context.getUnrelaxedAnswer().isType(RESULT)) {
                			if (!fullEval) {
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
            			

                        if (s instanceof Evaluable) {
                        	PipedInput<QCAVector> in = execute(new PFlow(pf, context), context, ((Evaluable) s).onCalcQuestion(), fullEval);
                            for (QCAVector v : in) {
                            	out.write(v);
                            }
                        } else if (s == null){
                            s = Statements.relationshipType(msg);
                            if (s instanceof Query || s instanceof Evaluable) {
                                PipedInput<QCAVector> in = Evaluator._.execute(new PFlow(pf, context), context);
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

			@Override
			public DisposingExecutor getQueue() {
				//System.out.println("onAnswer getQueue");
				return Executor.getFiber();
			}
        };
//        System.out.println("pf "+pf);
//        System.out.println("pf.answer.subscribe(onAnswer) "+pf.parent.answer);
		
        pf.parent.answer.subscribe(onAnswer);
		if (vector.getClosest().isType(FAKE))
	        pf.answer.subscribe(onAnswer);

        //send question to evaluation
        pf.question.publish(pf);
		
		//XXX: what to do with this?
        //reset.await(5, TimeUnit.SECONDS);
		
		return in;
	}
	
	public Subscribable<PFlow> onQuestion(final Relationship op) {
		return new OnQuestion();
	}
}
