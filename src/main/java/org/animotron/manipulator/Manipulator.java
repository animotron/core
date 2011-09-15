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
import org.neo4j.graphdb.PropertyContainer;
import org.neo4j.graphdb.Relationship;

import java.io.IOException;

import static org.animotron.Properties.RID;
import static org.animotron.graph.AnimoGraph.getDb;
import static org.animotron.graph.RelationshipTypes.REF;
import static org.animotron.graph.RelationshipTypes.RESULT;

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
	
	public final PipedInput execute(Relationship op) throws IOException {
		return execute(new PFlow(this, op), (PropertyContainer)op);
	}

	public final PipedInput execute(final PFlow pflow, Node op) throws IOException {
		return execute(pflow, (PropertyContainer)op);
	}

	public final PipedInput execute(final PFlow pflow, PropertyContainer op) throws IOException {
		return execute(pflow, op, null);
	}

	public final PipedInput execute(final PFlow pflow, PropertyContainer op, Subscribable<PFlow> sub) throws IOException {
        final PipedOutput out = new PipedOutput();
        PipedInput in = out.getInputStream();

        if (sub == null) {
			if (op instanceof Node) {
				sub = onQuestion(null);
			} else {
				sub = onQuestion((Relationship)op);
			}
        }
		
		if (sub == null) {
			out.write(op);
			out.close();
			return in;
		}
		
		final PFlow pf;
		if (op instanceof Node) {
			pf = new PFlow(pflow, (Node)op);
		} else {
			pf = new PFlow(pflow, (Relationship)op);
		}
		pf.question.subscribe(sub);

		
        //answers transfer to output
        Subscribable<Relationship> onAnswer = new Subscribable<Relationship>() {
            public void onMessage(Relationship msg) {
            	System.out.println("get answer "+msg);
            	try {
            		if (msg == null)
            			out.close();
            		else {
                        Statement s = null;
            			if (RESULT.equals(msg)) {
            				msg = getDb().getRelationshipById(
            						(Long)msg.getProperty(RID.name())
            					);
                            try {
                                s = Statements.name(THE._.reference(msg));
                            } catch (Exception e){}
            			} else if (REF.equals(msg)){
                            s = Statements.name(THE._.reference(msg));
                        }

                        if (s instanceof Evaluable) {
                        	PipedInput in = execute(pf, msg, ((Evaluable) s).onCalcQuestion());
                            for (Object obj : in) {
                                if (obj instanceof Relationship) {
                                    out.write(obj);
                                } else {
                                    System.out.println("UNHANDLED "+obj);
                                }
                            }
                        } else if (s == null){
                            s = Statements.relationshipType(msg.getType());
                            if (s instanceof Query || s instanceof Evaluable) {
                                try {
                                    PipedInput in = Evaluator._.execute(pf, msg);
                                    for (Object obj : in) {
                                        if (obj instanceof Relationship) {
                                            out.write(obj);
                                        } else {
                                            System.out.println("UNHANDLED "+obj);
                                        }
                                    }
                                } catch (Exception e) {
                                    //XXX: log error
                                    out.close();
                                }
                            } else {
                                out.write(msg);
                            }
                        } else {
                            out.write(msg);
                        }
            		}
				} catch (IOException e) {
					//XXX: what to do?
					e.printStackTrace();
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
		if (op instanceof Node)
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