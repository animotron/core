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
import java.util.Arrays;
import java.util.List;

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
	
	public final PipedInput<Relationship[]> execute(Relationship op) throws IOException {
		return execute(new PFlow(this), op);
	}

	public final PipedInput<Relationship[]> execute(final PFlow pflow, Node op) throws IOException {
		return execute(pflow, (PropertyContainer)op);
	}

	public final PipedInput<Relationship[]> execute(final PFlow pflow, PropertyContainer op) throws IOException {
		return execute(pflow, op, null);
	}

	public final PipedInput<Relationship[]> execute(final PFlow pflow, PropertyContainer op, Subscribable<PFlow> sub) throws IOException {
        final PipedOutput out = new PipedOutput();
        PipedInput<Relationship[]> in = out.getInputStream();

        if (sub == null) {
			if (op instanceof Node) {
				sub = onQuestion(null);
			} else {
				sub = onQuestion((Relationship)op);
			}
        }
		
		if (sub == null) {
			if (op instanceof Relationship) {
				out.write( new Relationship[] {(Relationship) op} );
			} else
				System.out.println("UNHANDLED op "+op);
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
        Subscribable<Relationship[]> onAnswer = new Subscribable<Relationship[]>() {
            public void onMessage(Relationship[] context) {
            	//System.out.println("get answer "+Arrays.toString(context));
            	try {
            		if (context == null) {

            			pf.countDown(out);

            		} else if (context[1] != null) {
            			int addedContexts = 0;
                        Statement s = null;
                        
                        Relationship msg = context[1];

//            			try {
//            				Relationship c = getDb().getRelationshipById(
//        						(Long)msg.getProperty(CID.name())
//        					);
//            				pf.addContextPoint(c);
//            				addedContexts++;
//            			} catch (Exception e) {
//						}
            			try {
            				Relationship r = getDb().getRelationshipById(
        						(Long)msg.getProperty(RID.name())
        					);
            				addedContexts += pf.addContextPoint(r);
            				
            				if (msg.isType(RESULT)) {
            					msg = r;

	                            try {
	                                s = Statements.name((String) THE._.reference(msg));
	                            } catch (Exception e){}
            				}
            			} catch (Exception e) {}

            			if (context[0] != null)
            				addedContexts += pf.addContextPoint(context[0]);
            				
            			if (msg.isType(REF)) {

                            s = Statements.name((String) THE._.reference(msg));

                        }

                        if (s instanceof Evaluable) {
                        	PipedInput<Relationship[]> in = execute(new PFlow(pf), msg, ((Evaluable) s).onCalcQuestion());
                            for (Object obj : in) {
                            	out.write(obj);
                            }
                        } else if (s == null){
                            s = Statements.relationshipType(msg);
                            if (s instanceof Query || s instanceof Evaluable) {
                                PipedInput<Relationship[]> in = Evaluator._.execute(new PFlow(pf), msg);
                                for (Object obj : in) {
                                    out.write(obj);
                                }
                            } else {
                                out.write(constructVector(pf, msg, addedContexts));
                            }
                        } else {
                            out.write(constructVector(pf, msg, addedContexts));
                        }
                        
                        while (addedContexts > 0) {
                        	pf.popContextPoint();
                        	addedContexts--;
                        }
                    } else {
                        //XXX: what to do if msg is null?
                        // out.close();
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
	
	private Relationship[] constructVector(PFlow pf, Relationship op, int count) {
    	List<Relationship> path = pf.getPFlowPath();
    	Relationship[] res = new Relationship[count+1];
    	for (int i = 0; i < count; i++) {
    		res[ count - 1 - i ] = path.get(i);                            		
    	}
    	res[0] = op;
        return res;
	}
	
	public Subscribable<PFlow> onQuestion(final Relationship op) {
		return new OnQuestion();
	}
}
