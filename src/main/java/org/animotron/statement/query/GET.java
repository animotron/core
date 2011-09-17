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
package org.animotron.statement.query;

import javolution.util.FastList;
import javolution.util.FastSet;
import org.animotron.Executor;
import org.animotron.graph.AnimoGraph;
import org.animotron.graph.AnimoPath;
import org.animotron.graph.GraphOperation;
import org.animotron.io.PipedInput;
import org.animotron.manipulator.Evaluator;
import org.animotron.manipulator.OnQuestion;
import org.animotron.manipulator.PFlow;
import org.animotron.statement.Statement;
import org.animotron.statement.Statements;
import org.animotron.statement.operator.*;
import org.animotron.statement.relation.HAVE;
import org.animotron.statement.relation.IS;
import org.animotron.statement.relation.USE;
import org.jetlang.channels.Subscribable;
import org.jetlang.core.DisposingExecutor;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Path;
import org.neo4j.graphdb.PropertyContainer;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.traversal.Evaluation;
import org.neo4j.graphdb.traversal.TraversalDescription;
import org.neo4j.kernel.Traversal;
import org.neo4j.kernel.Uniqueness;

import java.io.IOException;
import java.util.Arrays;
import java.util.List;
import java.util.Set;

import static org.animotron.graph.RelationshipTypes.REF;
import static org.neo4j.graphdb.Direction.OUTGOING;
import static org.neo4j.graphdb.traversal.Evaluation.*;

/**
 * Query operator 'Get'. Return 'have' relations on provided context.
 * 
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 */
public class GET extends Operator implements Evaluable, Query {

	public static final GET _ = new GET();
	
	private GET() { super("get"); }

	TraversalDescription td = Traversal.description().
			depthFirst().uniqueness(Uniqueness.RELATIONSHIP_PATH);

	private static TraversalDescription td_eval_IS = 
		Traversal.description().
			breadthFirst().
			relationships(IS._.relationshipType(), OUTGOING);
			//relationships(IC._.relationshipType(), OUTGOING);
	
	private static TraversalDescription td_eval_ic = 
			Traversal.description().
				breadthFirst().
				relationships(IS._.relationshipType(), OUTGOING).
				relationships(IC._.relationshipType(), OUTGOING);

	public OnQuestion onCalcQuestion() {
		return question;
	}
	
	//in-use by WITH
	public static Relationship getHaveAtPFlow(PFlow pf, String name) {
		final Path pflow = pf.getFlowPath();
		
		//XXX: remove when getFlowPath fixed
		if (pflow == null)
			return null;
		
		for (Relationship p : pflow.relationships()) {
			if (p.getType().name().equals(HAVE._.rType) && _.reference(p).equals(name)) {
				return p;
			}
		}
		return null;
	}

	private OnQuestion question = new OnQuestion() {

		@Override
		public void onMessage(final PFlow pf) {
			final Relationship op = pf.getOP();
			
			final Node node = op.getEndNode();
			final String name = reference(op);
			
			System.out.println("GET '"+ reference(op)+"'");

			//check, maybe, result was already calculated
			if (!Utils.results(node, pf)) {
				//no pre-calculated result, calculate it
				
				Subscribable<Relationship> onContext = new Subscribable<Relationship>() {
					@Override
					public void onMessage(Relationship context) {
						System.out.println("GET message ["+name+"] context "+context);
						
						if (context == null) {
							pf.countDown();
							return;
						}

//						Set<Relationship> res = getByTraversal(underHAVE, op, context, reference);
//						if (!res.isEmpty()) {
//							for (Relationship r : res)
//								pf.sendAnswer(r);
//
//							return;
//						}

						//final Relationship have = searchForHAVE(context, name);
						final Set<Relationship> rSet = get(pf, context, name);
						if (rSet != null) {
							for (Relationship r : rSet) {
								pf.sendAnswer(createResult(pf.getLastContext(), node, r, HAVE._.relationshipType));
							}
							return;
						}

//						PipedInput in;
//						try {
//							in = Evaluator._.execute(pf, context.getEndNode());
//						
//							for (Object n : in) {
//								//r = searchForHAVE((Relationship)n, name);
//								r = get(((Relationship)n).getEndNode(), reference);
//								
//								if (r != null) {
//									pf.sendAnswer(createResult(pf, node, r, HAVE._.relationshipType));
//								}
//							}
//						} catch (Exception e) {
//							//XXX: what to do?
//							e.printStackTrace();
//						}
					}

					@Override
					public DisposingExecutor getQueue() {
						return Executor.getFiber();
					}
				};
				pf.answer.subscribe(onContext);
				
				if (haveContext(pf)) {
					pf.addContextPoint(op);
					super.onMessage(pf);
				} else {
					System.out.println("P-FLOW is context for GET!\n pflow = "+pf.getFlowPath());
					pf.debug();
					
					for (Relationship st : pf.getPFlowPath()) {
						Set<Relationship> rSet = get(pf, st, name);
						if (rSet != null) {
							for (Relationship r : rSet) {
								pf.sendAnswer(createResult(pf.getLastContext(), node, r, HAVE._.relationshipType));
							}
							break;
						}
					}
					
//					Set<Relationship> res = getByTraversal(underHAVE, op, pf.getStartOP(), reference); //get(context.getEndNode(), reference);
//					if (!res.isEmpty())
//						for (Relationship r : res)
//							pf.sendAnswer(r);
				}
			}
			pf.await();
			pf.done();
		}
	};

	public Set<Relationship> get(PFlow pf, Relationship ref, final String name) {
		System.out.println("GET context = "+ref);
		
		Set<Relationship> set = new FastSet<Relationship>(); 

		Relationship have = null;
		
		List<Relationship> nextREFs = new FastList<Relationship>();
		nextREFs.add(ref);
		
		boolean first = true;
		
		while (true) {

			for (Relationship n : nextREFs) {
				System.out.println("checking "+n);
				have = searchForHAVE(n, name);
				if (have != null && !pf.isInStack(have)) 
					set.add(have);
			}
			
			if (set.size() > 0) return set;

			List<Relationship> newREFs = new FastList<Relationship>();

			for (Relationship n : nextREFs) {
				//System.out.println(""+n);
				//System.out.println("getStartNode OUTGOING");
				if (first || !n.equals(ref)) {
					for (Relationship r : n.getStartNode().getRelationships(OUTGOING)) {
						if (r.equals(n)) continue;
						
						//System.out.println(r);
						
						Statement st = Statements.relationshipType(r);
						if (st instanceof AN) {
							Relationship t = AN._.getREF(r);
							newREFs.add(t);
						} else if (st instanceof Reference) {
							try {
								PipedInput in = Evaluator._.execute(pf, r);
								
								for (Object rr : in) {
									if (rr instanceof Relationship) {
										newREFs.add((Relationship) rr);
										
									} else
										System.out.println("UNHANDLED "+rr);
									
								}
							} catch (IOException e) {
								// TODO Auto-generated catch block
								e.printStackTrace();
							}
						}
					}
				}
				first = false;

				//System.out.println("getEndNode OUTGOING");
				for (Relationship r : n.getEndNode().getRelationships(OUTGOING)) {
					//System.out.println(r);

					Statement st = Statements.relationshipType(r);
					if (st instanceof AN) {
						Relationship t = AN._.getREF(r);
						newREFs.add(t);

					} else if (st instanceof Reference) {
						if (!pf.isInStack(r)) {
							try {
								PipedInput in = Evaluator._.execute(new PFlow(pf, r), r);
								
								for (Object rr : in) {
									if (rr instanceof Relationship) {
										newREFs.add((Relationship) rr);
										
									} else
										System.out.println("UNHANDLED "+rr);
									
								}
							} catch (IOException e) {
								// TODO Auto-generated catch block
								e.printStackTrace();
							}
						}
					}
				}
				
			}

			if (newREFs.size() == 0) return null;
			
			nextREFs = newREFs;
		}
	}
	
	private Relationship searchForHAVE(final Relationship ref, final String name) {
		
		boolean checkStart = true;
		if (!REF.name().equals(ref.getType().name())) {
			System.out.println("WRONG WRONG WRONG WRONG ref = "+ref+" type "+ref.getType());
			checkStart = false;
		}
		
		Relationship have = null;
		
		//search for local 'HAVE'
		if (checkStart) {
			have = getByHave(ref.getStartNode(), name);
			if (have != null) return have;
		}

		//search for inside 'HAVE'
		have = getByHave(ref.getEndNode(), name);
		if (have != null) return have;

		//search 'IC' by 'IS' topology
		for (Relationship tdR : td_eval_IS.traverse(ref.getEndNode()).relationships()) {

			System.out.println("GET IC -> IS "+tdR);
			
			Relationship r = getByIC(tdR.getEndNode(), name);
			if (r != null) return r;
			
			//search for have
			have = getByHave(tdR.getEndNode(), name);
			if (have != null) return have;
		}
		
		return null;
	}

	//in-use by SELF
	public Relationship get(Node context, final String name) {
		
		//System.out.println("GET get context = "+context);

		//search for local 'HAVE'
		Relationship have = getByHave(context, name);
		if (have != null) return have;

		Relationship instance = context.getSingleRelationship(REF, OUTGOING);
		if (instance != null) {
			//change context to the-instance by REF
			context = instance.getEndNode();
			
			//search for have
			have = getByHave(context, name);
			if (have != null) return have;
		}
		
		Relationship prevTHE = null;
		
		//search 'IC' by 'IS' topology
		for (Relationship tdR : td_eval_ic.traverse(context).relationships()) {
			
			Statement st = Statements.relationshipType(tdR.getType());
			if (st instanceof IS) {
				//System.out.println("GET IC -> IS "+tdR);
				if (prevTHE != null) {
					//search for have
					have = getByHave(prevTHE.getEndNode(), name);
					if (have != null) return have;
				}
				prevTHE = tdR;
				
			} else if (st instanceof IC) {
				//System.out.print("GET IC -> "+tdR);
				
				if (name.equals(reference(tdR))) {
					//System.out.println(" MATCH");
					
					//store
					final Node sNode = context;
					final Relationship r = tdR;

					return AnimoGraph.execute(new GraphOperation<Relationship>() {
						@Override
						public Relationship execute() {
							Relationship res = sNode.createRelationshipTo(r.getEndNode(), HAVE._.relationshipType());
							//RID.set(res, r.getId());
							return res;
						}
					});
					
					//in-memory
					//Relationship res = new InMemoryRelationship(context, tdR.getEndNode(), HAVE._.relationshipType());
					//RID.set(res, tdR.getId());
					//return res;
					
					//as it
					//return tdR;
				}
				System.out.println();
			}
		}
		
		if (prevTHE != null) {
			//search for have
			have = getByHave(prevTHE.getEndNode(), name);
			if (have != null) return have;
		}

		return null;
	}
	
	private Relationship getByHave(final Node context, final String name) {
		final Node target = THE._.get(name).getEndNode();
		
		TraversalDescription trav = td.
		evaluator(new Searcher(){
			@Override
			public Evaluation evaluate(Path path) {
				return _evaluate_(path, target, HAVE._.rType);
			}
		});

		Relationship res = null;
		for (Path path : trav.traverse(context)) {
			//TODO: check that this is only one answer
			//System.out.println(path);
			for (Relationship r : path.relationships()) {
				res = r;
				break;
			}
		}
		return res;
	}
	
	private Relationship getByIC(final Node context, final String name) {
		final Node target = THE._.get(name).getEndNode();
		
		TraversalDescription trav = td.
		evaluator(new Searcher(){
			@Override
			public Evaluation evaluate(Path path) {
				return _evaluate_(path, target, IC._.rType);
			}
		});

		Relationship res = null;
		for (Path path : trav.traverse(context)) {
			//TODO: check that this is only one answer
			//System.out.println(path);
			for (Relationship r : path.relationships()) {
				res = r;
				break;
			}
		}
		return res;
	}

	public Set<Relationship> getByTraversal(final Relationship underHAVE, final Relationship start_op, final PropertyContainer context, final String name) {
		
		TraversalDescription td;
		
		if (context instanceof Relationship) {
//			EmptyGetContextFinder contextFinder = new EmptyGetContextFinder();
//			contextFinder.findHaves(Utils.getByREF(start_op.getEndNode()), (Relationship)context);
			
			td = Traversal.description().depthFirst().
			uniqueness(Uniqueness.RELATIONSHIP_PATH).
			evaluator(new org.neo4j.graphdb.traversal.Evaluator(){
				@Override
				public Evaluation evaluate(Path path) {
					if (path.length() > 0) {
						Relationship r = path.lastRelationship(); 
						if (r.getStartNode().equals(path.endNode())) {
							if (r.equals(context)) {
								return INCLUDE_AND_PRUNE;
							} 

//							String rType = r.getType().reference();
							
							//second must be REF
							// or it can be IS ! so, REF and HAVE after IS
//							if (path.length() == 1 && !(rType.equals(RelationshipTypes.REF.reference()) || rType.equals(IS._.rType)) ) {
//								return EXCLUDE_AND_PRUNE;
//							} else if (path.length() == 2 && !(rType.equals(HAVE._.rType) || rType.equals(IC._.rType))) {
//								return EXCLUDE_AND_PRUNE;
//							}
							return EXCLUDE_AND_CONTINUE;	
						//Allow ...<-IS->...
						} if (path.length() > 2 && r.getType().name().equals(IS._.rType)) {
							return EXCLUDE_AND_CONTINUE;
						}
						return EXCLUDE_AND_PRUNE;
					}
					return EXCLUDE_AND_CONTINUE;
				}
			});
		} else {
			System.out.println("build traversal with context = "+context);
			
			//TODO: merge with prev. one
			td = Traversal.description().depthFirst().
			uniqueness(Uniqueness.RELATIONSHIP_PATH).
			evaluator(new org.neo4j.graphdb.traversal.Evaluator(){
				@Override
				public Evaluation evaluate(Path path) {
					//System.out.println(path);
					if (path.length() > 0) {
						Relationship r = path.lastRelationship(); 
						if (r.getStartNode().equals(path.endNode())) {
							if (r.getStartNode().equals(context)) {
								return INCLUDE_AND_PRUNE;
							}
							return EXCLUDE_AND_CONTINUE;	
						//Allow ...<-IS->...
						} if (path.length() > 2 && r.getType().name().equals(IS._.rType)) {
							return EXCLUDE_AND_CONTINUE;
						} 
						return EXCLUDE_AND_PRUNE;
					}
					return EXCLUDE_AND_CONTINUE;
				}
			});
		}
		
		//System.out.println("context = "+context+" start_op = "+start_op);
		
		Node node = Utils.getByREF(start_op.getEndNode());
//		for (Path path : td.traverse(node)) {
//			System.out.println("path = "+path);
//		}
		
		int deep = Integer.MAX_VALUE;
		Set<Relationship> result = new FastSet<Relationship>();

		Relationship thisResult = null;
		int thisDeep = 0;
		
		for (Path path : td.traverse(node)) {
			System.out.println("path = "+path);
			System.out.println("path = "+new AnimoPath(path));
			
			boolean foundIS = false;
			boolean foundBackIS = false;
			Relationship foundIC = null;
			Node lastNode = null;
			thisDeep = 0;
			
			int step = 0;
			
			boolean REFcase = false;
			for (Relationship r : path.relationships()) {
				step++;
				
				String type = r.getType().name();

				if (thisDeep > 0) {

					if (type.equals(IS._.relationshipType().name()) && r.getStartNode().equals(lastNode)) {
						lastNode = r.getEndNode();
						foundBackIS = true;
					} else {
						lastNode = r.getStartNode();
					}

					if (type.equals(USE._.rType)) {
						thisDeep += 2;
					} else  if (type.equals(IS._.rType)) { //type.equals(REF.reference()) ||
						if (!REFcase) {
							REFcase = true;
							thisDeep++;
						}
						continue;
					}

					REFcase = false;
					
					thisDeep++;
					continue;
				}
				
				if (step == 1 && type.equals(REF.name())) {
					REFcase = true;
				} else if (REFcase && step == 2 && !(type.equals(HAVE._.rType) || type.equals(IC._.rType))) {
					break;
				}
				
				lastNode = r.getStartNode();
				
				if (type.equals(IS._.relationshipType().name())) {
					if (name.equals(IS._.reference(r))) {
						foundIS = true;
						continue;
					}
					
					if (foundIC != null) {
						thisResult = ICresult(context, foundIC);
						thisDeep++;
						REFcase = false;
						continue;
					}
					
				} else if (type.equals(HAVE._.relationshipType().name()) && (name.equals(HAVE._.reference(r)) || foundIS)) {
					//ignore empty have 
					if (!Utils.haveContext(r.getEndNode()))
						break;
					
					//cycle detection
					if (underHAVE != null && r.equals(underHAVE))
						break;
					
					thisResult = r;
					thisDeep++;
					REFcase = false;
				
				} else if (type.equals(IC._.relationshipType().name()) && (name.equals(IC._.reference(r)) || foundIS)) {
					if (foundIS) {
						thisResult = ICresult(context, r);
						thisDeep++;
						REFcase = false;
					} else {
						foundIC = r;
					}
				}
			}
			
			if (thisDeep == 0)
				;
			else if (thisDeep == deep && !foundBackIS) {
				System.out.println("Adding thisDeep = "+thisDeep+"; deep = "+deep);
				result.add(thisResult);

			} else if (thisDeep < deep) {
				System.out.println("Creating thisDeep = "+thisDeep+"; deep = "+deep);
				result.clear();
				result.add(thisResult);
				deep = thisDeep;
			}
		}
		
		System.out.println(Arrays.toString(result.toArray()));
		
		return result;
	}
	
	private Relationship ICresult(PropertyContainer context, Relationship r) {
		//store
		final Node sNode;
		if (context instanceof Relationship) {
			sNode = ((Relationship)context).getEndNode();
		} else {
			sNode = (Node)context;
		}
		final Node eNode = r.getEndNode();
		
		//avoid duplicate
		for (Relationship h : sNode.getRelationships(HAVE._.relationshipType(), OUTGOING)) {
			if (h.getEndNode().equals(eNode))
				return h;
		}

		return AnimoGraph.execute(new GraphOperation<Relationship>() {
			@Override
			public Relationship execute() {
				Relationship res = sNode.createRelationshipTo(eNode, HAVE._.relationshipType());
				//RID.set(res, r.getId());
				return res;
			}
		});
	}

	abstract class Searcher implements org.neo4j.graphdb.traversal.Evaluator {

		public Evaluation _evaluate_(Path path, Node target, String type) {
			//System.out.println(path);
			
			if (path.length() == 0)
				return EXCLUDE_AND_CONTINUE;
			
			Relationship r = path.lastRelationship(); 
			String rType = r.getType().name();

			if (path.length() == 1) {
				if (rType.equals(type))
					return EXCLUDE_AND_CONTINUE;
					
				return EXCLUDE_AND_PRUNE;
				
			} else if (path.length() >= 2) {
				if (rType.equals(REF.name())) {
					Node node = r.getEndNode();
					if (target.equals(node)) 
						return INCLUDE_AND_PRUNE;

					return EXCLUDE_AND_CONTINUE;
				
				//XXX: check direction!
				} else if (rType.equals(IS._.rType)) {
					if (r.getEndNode().equals(path.endNode())) {
						return EXCLUDE_AND_CONTINUE;
					}
					return EXCLUDE_AND_PRUNE;
				}
			}
			return EXCLUDE_AND_PRUNE;
		}
	}
}
