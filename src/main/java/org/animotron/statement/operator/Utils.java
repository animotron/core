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
package org.animotron.statement.operator;

import org.animotron.Executor;
import org.animotron.exception.AnimoException;
import org.animotron.graph.Properties;
import org.animotron.graph.AnimoGraph;
import org.animotron.graph.GraphOperation;
import org.animotron.graph.index.Order;
import org.animotron.graph.index.Result;
import org.animotron.io.Pipe;
import org.animotron.manipulator.Evaluator;
import org.animotron.manipulator.PFlow;
import org.animotron.manipulator.QCAVector;
import org.animotron.statement.Statement;
import org.animotron.statement.Statements;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Path;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.RelationshipType;
import org.neo4j.graphdb.index.IndexHits;
import org.neo4j.graphdb.traversal.Evaluation;
import org.neo4j.graphdb.traversal.TraversalDescription;
import org.neo4j.kernel.Traversal;

import java.io.IOException;
import java.util.List;
import java.util.Set;

import static org.animotron.graph.Properties.CONTEXT;
import static org.animotron.graph.Properties.RID;
import static org.animotron.graph.AnimoGraph.getDb;
import static org.animotron.graph.RelationshipTypes.RESULT;
import static org.neo4j.graphdb.Direction.INCOMING;
import static org.neo4j.graphdb.Direction.OUTGOING;
import static org.neo4j.graphdb.traversal.Evaluation.*;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class Utils {

	public static TraversalDescription td_RESULT =
		Traversal.description().
			breadthFirst().
			relationships(RESULT, OUTGOING );

	public static TraversalDescription td_eval_IS = 
			Traversal.description().
				breadthFirst().
				relationships(AN._, OUTGOING);
		
	public static TraversalDescription upIS = 
			Traversal.description().
				breadthFirst().
				relationships(AN._, INCOMING);

	public static TraversalDescription td_THE = 
			Traversal.description().
				breadthFirst().
				relationships(AN._, INCOMING).
				relationships(THE._, INCOMING).
	            evaluator(new org.neo4j.graphdb.traversal.Evaluator(){
	    			@Override
	    			public Evaluation evaluate(Path path) {

	    				if (path.length() == 0)
	    					return EXCLUDE_AND_CONTINUE;
	    				
	    				if (!path.endNode().equals(path.lastRelationship().getStartNode()))
	    					return EXCLUDE_AND_PRUNE;
	    				
	    				if (path.lastRelationship().isType(org.animotron.statement.operator.REF._) 
	    						|| path.lastRelationship().isType(RESULT))
	    					return EXCLUDE_AND_PRUNE;
	    				

    					if (path.lastRelationship().isType(THE._))
	    					return INCLUDE_AND_CONTINUE;
	    				
	    				return EXCLUDE_AND_CONTINUE;
	    			}
	    		});

	
	public static Pipe getByREF(final PFlow pf) {
		return getByREF(pf, pf.getVector());
	}

	public static Pipe getByREF(final PFlow pf, final QCAVector vector) {
		final Pipe pipe = Pipe.newInstance(); 
		
		final Node node = vector.getClosest().getEndNode();
		//System.out.println(node);
		
		Executor.execute(new Runnable() {
			@Override
			public void run() {
				try {
					Relationship theNode = THE._.get((String)THE._.reference(node));
		
					if (theNode != null) {
						pipe.write(vector.answered(theNode));
						pipe.close();
					}
				} catch (Exception e) {
				}
					
				if (pf != null) {
					Relationship first = null;
					IndexHits<Relationship> hits = Order.queryDown(node);
					try {
						for (Relationship res : hits) {
							if (res.isType(org.animotron.statement.operator.REF._) || first == null) {
								evaluable(pf, vector.question(res), pipe);
								if (first == null)
									first = res;
							} else
								break;
						}
						
					} catch (Exception e) {
						pf.sendException(e);
						
					} finally {
						hits.close();
						pipe.close();
					}
				}
			}
		});

		return pipe;
	}

	private static Pipe evaluable(final PFlow pf, final QCAVector v, final Pipe pipe) throws InterruptedException, IOException, AnimoException {
		
		Relationship r = v.getClosest();
		Statement s = Statements.relationshipType(r);
		if (s instanceof Query || s instanceof Evaluable) {
			//System.out.println("+++++++++++++++++++++++++++++++++++++++++ get evaluable");
			Pipe in = Evaluator._.execute(v);
			for (QCAVector e : in) {
				
                Statement aS = Statements.relationshipType(e.getAnswer());
				if (!(aS instanceof Evaluable && !(s instanceof Shift))) {
//				if (result.isType(REF) 
//						|| result.isType(org.animotron.statement.operator.REF._)
//						|| result.isType(THE._)
//					) {
					pipe.write(e);
				} else {
					Pipe p = eval(e);
					QCAVector rr;
					while ((rr = p.take()) != null) {
						pipe.write(rr);
					}
				}
			}
			//System.out.println("end++++++++++++++++++++++++++++++++++++++ get evaluable");
		} else {
			pipe.write(v.getContext().get(0).answered(r));
		}
		
		return pipe;
	}
	
	public static Pipe getTheRelationships(final PFlow pf, final QCAVector v) throws IOException {
		final Pipe pipe = Pipe.newInstance(); 

		Executor.execute( new Runnable() {
			@Override
			public void run() {
				getTheRelationships(pf, v, pipe);
				pipe.close();
			}
		});
		
		return pipe;
	}

	public static void getTheRelationships(PFlow pf, QCAVector v, Pipe pipe) {
		try {
		
			Statement s = Statements.relationshipType(v.getClosest());
			if (s instanceof AN) {
                try {
//                	for (QCAVector a : AN._.getREFs(pf, v)) {
//                		out.write(a);
//                	}
                    s = Statements.name((String) THE._.reference(v.getClosest()));
                    
                } catch (Exception e) {
    				pipe.write(v);//.answered(v.getClosest())
    				return;
                }
			}

			if (s instanceof Query || s instanceof Evaluable) {
				//System.out.println("+++++++++++++++++++++++++++++++++++++++++ get evaluable");
				Pipe in = Evaluator._.execute(v);
				QCAVector e;
				while ((e = in.take()) != null) {

					Relationship result = e.getAnswer();
					
					if (result.isType(REF._) || result.isType(THE._)) {
						pipe.write(e);
					} else {
						Pipe p = eval(e);
						QCAVector rr;
						while ((rr = p.take()) != null) {
							pipe.write(rr);
						}
					}
				}
				//System.out.println("end++++++++++++++++++++++++++++++++++++++ get evaluable");
			} else {
				pipe.write(v);//.answered(v.getClosest())
			}
		} catch (Exception e) {
			e.printStackTrace();
			pf.sendException(e);
		}
	}
	
	public static Pipe eval(final QCAVector vector) throws IOException, AnimoException, InterruptedException {
        final Pipe pipe = Pipe.newInstance();
        
        Executor.execute(new Runnable() {
			@Override
			public void run() {
		        Relationship op = vector.getClosest();
		        
		        QCAVector prev = null;

				IndexHits<Relationship> hits = Order.queryDown(op.getEndNode());
				try {
					for (Relationship rr : hits) {
						if (rr.isType(REF._)) continue;
						
						Statement _s = Statements.relationshipType(rr);
						if (_s instanceof Query || _s instanceof Evaluable) {
							prev = vector.question(rr, prev);
							Pipe _in = Evaluator._.execute(prev);
							for (QCAVector ee : _in) {
								//XXX: ee should be context too???
								pipe.write(new QCAVector(op, vector, ee.getUnrelaxedAnswer()));
							}
							
						} else {
							pipe.write(new QCAVector(op, vector, rr));
						}
					}
				} catch (Exception e) {
					//XXX: logs
					e.printStackTrace();
				} finally {
					hits.close();
					pipe.close();
				}
			}
        });
		//out.close();
		
		return pipe;
	}

	@Deprecated //???
	public static Relationship getByREF(final Relationship r) {
		final Relationship res = Order.first(1, r.getEndNode())[0];
		
		return res;
	}

	public static boolean results(PFlow pf) {
		return false;
		//return results(pf.getOP(), pf, pf.getPathHash());
	}

	public static boolean results(PFlow pf, byte[] hash) {
		return false;
		//return results(pf.getOP(), pf, hash);
	}

	public static boolean results(Relationship op, PFlow pf) {
		return false;
		//return results(op, pf, pf.getPathHash());
	}

	public static boolean results(Relationship op, PFlow pf, byte[] hash) {
		return false;
//		boolean haveSome = false;
//
//		//System.out.println("check index "+r+" "+pf.getPathHash()[0]+" "+pf.getPFlowPath());
//		for (QCAVector v : Result.get(hash, op)) {
//			pf.sendAnswer(v);
//			
//			haveSome = true;
//		}
//		return haveSome;
	}
	
	public static boolean haveContext(PFlow pf) {
		return haveContext(pf.getOPNode());
	}
	
	public static boolean haveContext(Node node) {
		return CONTEXT.has(node);
	}

	public static Relationship relax(Relationship relation) {
		Relationship r = relation;
		while (true) {
			try {
	        	r = getDb().getRelationshipById(
	                (Long)r.getProperty(RID.name())
	            );
			} catch (Exception ex) {
				break;
			}
		}
		return r;
	}

	public static long relaxedId(Relationship relation) {
		try {
            return (Long)relation.getProperty(RID.name());
		} catch (Exception ex) {
		}
		return -1;
	}

	public static Relationship createResult(final PFlow pf, final Node node, final Relationship r, final RelationshipType rType) {
		return createResult(pf, null, node, r, rType, pf.getPathHash());
	}

	public static Relationship createResult(final PFlow pf, final List<QCAVector> context, final Node node, final Relationship r, final RelationshipType rType) {
		return createResult(pf, context, node, r, rType, pf.getPathHash());
	}

	public static Relationship createResult(final PFlow pf, final Node node, final Relationship r, final RelationshipType rType, final byte[] hash) {
		return createResult(pf, null, node, r, rType, hash);
	}

	public static Relationship createResult(final PFlow pf, final List<QCAVector> context, final Node node, final Relationship r, final RelationshipType rType, final byte[] hash) {
		return AnimoGraph.execute(new GraphOperation<Relationship>() {
			@Override
			public Relationship execute() {
				//check if it exist
				Relationship res = Result.getIfExist(node, r, rType);
				if (res != null) {
					Result.add(res, hash);
					
					//for debug
					//CID.set(res, context.getId());
					
					return res;
				}
				
				//adding if not
				res = node.createRelationshipTo(r.getEndNode(), rType);
				//store to relationship arrow
				RID.set(res, r.getId());
				//for debug
//				if (context != null) {
//					if (context.size() > 1) System.out.println("WARNING ... more then one context for CID");
//					//XXX: rewrite!
//					for (QCAVector c : context) {
//						try {
//							CID.set(res, c.mashup());
//						} catch (Exception e) {
//						}
//					}
//				}
				Result.add(res, hash);
				//System.out.println("add to index "+r+" "+pf.getPathHash()[0]+" "+pf.getPFlowPath());
				return res;
			}
		});
	}

	public static void debug(Statement st, PFlow pf) {
		System.out.print(st.name()+" "+pf.getOP().getId()+" ");
		for (QCAVector v : Utils.getByREF(pf)) {
			Node theNode = v.getUnrelaxedClosest().getEndNode();
			try {
				System.out.print("'"+name(theNode)+"'");
			} catch (Exception e) {
				System.out.print("???");
			}
			System.out.print(" ["+theNode+"], ");
		}
		System.out.println();
	}
	
	public static void debug(Relationship r) {
		System.out.print(r.getType()+" ");
		Node theNode = r.getEndNode();
		try {
			System.out.print("'"+name(theNode)+"'");
		} catch (Exception e) {
			System.out.print("???");
		}
		System.out.println(" ["+theNode+"]");
	}

	public static void debug(Statement st, Relationship op, Set<Node> thes) {
		System.out.print(st.name()+" "+op.getId()+" ");
		if (thes.isEmpty())
			System.out.print("no the-nodes in bag!");
		else {
			for (Node theNode : thes) {
				try {
					System.out.print("'"+name(theNode)+"'");
				} catch (Exception e) {
					System.out.print("???");
				}
				System.out.print(" ["+theNode+"], ");
			}
		}
		System.out.println();
	}
	
    public static String name(Node theNode) {
    	return (String) Properties.NAME.get(theNode);
    }
}
