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

import javolution.util.FastMap;
import javolution.util.FastSet;
import org.animotron.Executor;
import org.animotron.graph.AnimoGraph;
import org.animotron.graph.GraphOperation;
import org.animotron.graph.index.Order;
import org.animotron.io.PipedInput;
import org.animotron.manipulator.Evaluator;
import org.animotron.manipulator.OnQuestion;
import org.animotron.manipulator.PFlow;
import org.animotron.manipulator.QCAVector;
import org.animotron.statement.Statement;
import org.animotron.statement.Statements;
import org.animotron.statement.operator.*;
import org.animotron.statement.relation.SHALL;
import org.jetlang.channels.Subscribable;
import org.jetlang.core.DisposingExecutor;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Path;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.index.IndexHits;
import org.neo4j.graphdb.traversal.Evaluation;
import org.neo4j.graphdb.traversal.TraversalDescription;
import org.neo4j.kernel.Traversal;
import org.neo4j.kernel.Uniqueness;

import java.io.IOException;
import java.util.Map;
import java.util.Set;

import static org.animotron.Properties.RID;
import static org.neo4j.graphdb.Direction.OUTGOING;
import static org.animotron.graph.RelationshipTypes.RESULT;

/**
 * Query operator 'Get'. Return 'have' relations on provided context.
 * 
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 */
public class GET extends AbstractQuery implements Evaluable, Shift {

	public static final GET _ = new GET();
	
	private static boolean debug = true;
	
	private GET() { super("get"); }

	TraversalDescription td = Traversal.description().
			depthFirst().uniqueness(Uniqueness.RELATIONSHIP_PATH);

	private static TraversalDescription td_eval_ic = 
			Traversal.description().
				breadthFirst().
				relationships(AN._, OUTGOING).
				relationships(SHALL._, OUTGOING);

	public OnQuestion onCalcQuestion() {
		return question;
	}
	
	private OnQuestion question = new OnQuestion() {

		@Override
		public void onMessage(final PFlow pf) {
			final Relationship op = pf.getOP();
			
			final Node node = op.getEndNode();
			
			final Set<Relationship> visitedREFs = new FastSet<Relationship>();

			final Set<Node> thes = new FastSet<Node>(); 
			
			for (QCAVector theNode : AN.getREFs(pf, op)) {
				thes.add(theNode.getAnswer().getEndNode());
			}

			evalGet(pf, op, node, thes, visitedREFs);
			
			pf.await();
			pf.done();
		}

		private void evalGet(
				final PFlow pf, 
				final Relationship op, 
				final Node node, 
				final Set<Node> thes, 
				final Set<Relationship> visitedREFs) {
			
			Utils.debug(GET._, op, thes);

			//check, maybe, result was already calculated
			if (!Utils.results(pf)) {
				//no pre-calculated result, calculate it
				
				Subscribable<QCAVector> onContext = new Subscribable<QCAVector>() {
					@Override
					public void onMessage(QCAVector vector) {
						if (debug) System.out.println("GET ["+op+"] vector "+vector);
						
						if (vector == null) {
							pf.countDown();
							return;
						}
						
						final Set<QCAVector> rSet = get(pf, op, vector, thes, visitedREFs);
						if (rSet != null) {
							for (QCAVector v : rSet) {
								pf.sendAnswer(v, RESULT);//, AN._); //XXX: change to AN
							}
						}
					}

					@Override
					public DisposingExecutor getQueue() {
						return Executor.getFiber();
					}
				};
				pf.answer.subscribe(onContext);
				
				if (Utils.haveContext(pf)) {
					super.onMessage(pf);
				} else {
					
					boolean first = true;
					Set<QCAVector> rSet = null;
					for (QCAVector vector : pf.getPFlowPath()) {
						System.out.println("CHECK PFLOW "+vector);
						if (first) {
							first = false;

							if (vector.getContext() == null) continue;
							
							Set<QCAVector> refs = new FastSet<QCAVector>();
							for (QCAVector v : vector.getContext()) {
								refs.add(v);
								
							}
							rSet = get(pf, op, refs, thes, visitedREFs); 
							
						} else {
							rSet = get(pf, op, vector, thes, visitedREFs);
						}
						if (rSet != null) {
							for (QCAVector v : rSet) {
								pf.sendAnswer(v, RESULT);//, AN._); //XXX: change to AN
							}
							break;
						}
					}
				}
			}
		};

	};
	
	public Set<QCAVector> get(PFlow pf, Relationship op, QCAVector vector, final Set<Node> thes, Set<Relationship> visitedREFs) {
		Set<QCAVector> refs = new FastSet<QCAVector>();
		refs.add(vector);
		
		return get(pf, op, refs, thes, visitedREFs); 
	}

	public Set<QCAVector> get(final PFlow pf, Relationship op, Node ref, final Set<Node> thes, final Set<Relationship> visitedREFs) {
		Set<QCAVector> set = new FastSet<QCAVector>();
		
		Relationship[] have = searchForHAVE(pf, null, ref, thes);
		if (have != null) { 
			for (int i = 0; i < have.length; i++) { 
				if (!pf.isInStack(have[i]))
					set.add(new QCAVector(pf.getOP(), have[i]));
			}
		}
		
		if (!set.isEmpty()) return set;

		Set<QCAVector> newREFs = new FastSet<QCAVector>();
		getOutgoingReferences(pf, null, ref, newREFs, null);
		
		return get(pf, op, newREFs, thes, visitedREFs); 
	}
	
	private boolean check(Set<QCAVector> set, final PFlow pf, final Relationship op, final QCAVector v, final Relationship toCheck, final Set<Node> thes, Set<Relationship> visitedREFs) {
		if (toCheck == null) return false;
		
		visitedREFs.add(toCheck);
		
		Relationship[] have = searchForHAVE(pf, toCheck, v, thes);
		if (have != null) {
			boolean added = false;
			for (int i = 0; i < have.length; i++) { 
				if (!pf.isInStack(have[i])) {
					set.add(new QCAVector(op, v, have[i]));
					added = true;
				}
			}
			return added;
		}
		return false;
	}

	public Set<QCAVector> get(
			final PFlow pf,
			final Relationship op,
			final Set<QCAVector> REFs, 
			final Set<Node> thes, 
			Set<Relationship> visitedREFs) {
		
		//System.out.println("GET context = "+ref);
		
		if (visitedREFs == null) visitedREFs = new FastSet<Relationship>();
		
		Set<QCAVector> set = new FastSet<QCAVector>();
		
		Set<QCAVector> nextREFs = new FastSet<QCAVector>();
		nextREFs.addAll(REFs);

		//boolean first = true;
		
		Relationship t = null;
		
		while (true) {
			
			if (debug) System.out.println("nextREFs ");//+Arrays.toString(nextREFs.toArray()));

			for (QCAVector v : nextREFs) {
				if (debug) System.out.println("checking "+v);
				if (!check(set, pf, op, v, v.getUnrelaxedAnswer(), thes, visitedREFs)) {
					check(set, pf, op, v, v.getQuestion(), thes, visitedREFs);
				}
			}
			
			if (set.size() > 0) return set;

			Set<QCAVector> newREFs = new FastSet<QCAVector>();

			for (QCAVector vector : nextREFs) {
				QCAVector[] cs = vector.getContext();
				if (cs != null) {
					for (QCAVector c : cs) {
						t = c.getUnrelaxedAnswer();
						if (t != null && !visitedREFs.contains(t))
							newREFs.add(c);
						else {
							t = c.getQuestion();
							if (!visitedREFs.contains(t))
								newREFs.add(c);
						}
					}
				}
				
				
//				Relationship n = vector.getClosest();
//				//System.out.println(""+n);
//				//System.out.println("getStartNode OUTGOING");
//				if (first || !REFs.contains(n)) {
//					IndexHits<Relationship> it = Order.queryDown(n.getStartNode());
//					try {
//						for (Relationship r : it) {
//							if (r.equals(n)) continue;
//							//System.out.println(r);
//							
//							Statement st = Statements.relationshipType(r);
//							if (st instanceof AN) {
//								for (QCAVector v : AN.getREFs(pf, r)) {
//									Relationship t = v.getAnswer();
//									if (!visitedREFs.contains(t))
//										newREFs.add(v);
//								}
//							} else if (st instanceof Reference) {
//								try {
//									if (!pf.isInStack(r)) {
//										PipedInput<QCAVector> in = Evaluator._.execute(new PFlow(pf), r);
//										
//										for (QCAVector rr : in) {
//											if (!visitedREFs.contains(rr.getAnswer()))
//												newREFs.add(rr);
//										}
//									}
//								} catch (IOException e) {
//									// TODO Auto-generated catch block
//									e.printStackTrace();
//								}
//							}
//						}
//					} finally {
//						it.close();
//					}
//				}
//				first = false;
//
//				//System.out.println("getEndNode OUTGOING");
				t = vector.getUnrelaxedAnswer();
				if (t != null) {
					if (! t.isType(AN._))
						getOutgoingReferences(pf, t, t.getStartNode(), newREFs, visitedREFs);
					
					getOutgoingReferences(pf, t, t.getEndNode(), newREFs, visitedREFs);
				}
			}

			if (newREFs.size() == 0) return null;
			
			nextREFs = newREFs;
		}
	}
	
	private void getOutgoingReferences(PFlow pf, Relationship rr, Node node, Set<QCAVector> newREFs, Set<Relationship> visitedREFs) {

		IndexHits<Relationship> it = Order.queryDown(node);
		try {
			boolean first = rr == null || !rr.isType(REF._);
			for (Relationship r : it) {
				//System.out.println(r);
				
				if (first) {
					first = false;
					continue;
				}
				if (r.isType(REF._)) continue;
	
				Statement st = Statements.relationshipType(r);
				if (st instanceof AN) {
					//System.out.println(r);
					for (QCAVector v : AN.getREFs(pf, r)) {
						Relationship t = v.getAnswer();
						//System.out.println(t);
						if (visitedREFs != null && !visitedREFs.contains(t))
							newREFs.add(v);
					}
	
				} else if (st instanceof Reference) {
					if (!pf.isInStack(r)) {
						try {
							PipedInput<QCAVector> in = Evaluator._.execute(new PFlow(pf, r), r);
							
							for (QCAVector v : in) {
								if (visitedREFs != null && !visitedREFs.contains(v.getAnswer()))
									newREFs.add(v);
							}
						} catch (IOException e) {
							// TODO Auto-generated catch block
							e.printStackTrace();
						}
					}
				}
			}
		} catch (Exception e) {
			pf.sendException(e);
		} finally {
			it.close();
		}
	}
	
	private Relationship[] searchForHAVE(
			final PFlow pf, 
			final Relationship ref,
			final QCAVector v,
			final Set<Node> thes) {
		
		if (ref.isType(REF._) && thes.contains(ref.getEndNode())) {
			return new Relationship[] {v.getQuestion()};
		}
		
		boolean checkStart = true;
		if (ref.isType(AN._)) {
			checkStart = false;
		}
		
		Relationship[] have = null;
		
		//search for inside 'HAVE'
		//return searchForHAVE(pf, ref, ref.getEndNode(), thes);
		have = getByHave(pf, ref, ref.getEndNode(), thes);
		if (have != null) return have;

		//search for local 'HAVE'
		if (checkStart) {
			have = getByHave(pf, null, ref.getStartNode(), thes);
			if (have != null) return have;
		}

		return null;
	}

	private Relationship[] searchForHAVE(final PFlow pflow, Relationship op, final Node ref, final Set<Node> thes) {
		
		Relationship[] have = null;

		//search for inside 'HAVE'
		have = getByHave(pflow, op, ref, thes);
		if (have != null) return have;

		//search 'IC' by 'IS' topology
		for (Relationship tdR : Utils.td_eval_IS.traverse(ref).relationships()) {

			//System.out.println("GET IC -> IS "+tdR);
			
			Relationship r = getShall(tdR.getEndNode(), thes);
			if (r != null) {
				final Node sNode = ref;
				final Node eNode = r.getEndNode();
				final long id = r.getId();
				return new Relationship[] {
					AnimoGraph.execute(new GraphOperation<Relationship>() {
						@Override
						public Relationship execute() {
							Relationship res = sNode.createRelationshipTo(eNode, AN._);
							RID.set(res, id);
							return res;
						}
					})	
				};
			}
			
			//search for have
			have = getByHave(pflow, tdR, tdR.getEndNode(), thes);
			if (have != null) return have;
		}
		
		return null;
	}
	
	//XXX: in-use by SELF
	public Relationship[] getBySELF(final PFlow pf, Node context, final Set<Node> thes) {
		
		//System.out.println("GET get context = "+context);

		//search for local 'HAVE'
		Relationship[] have = getByHave(pf, null, context, thes);
		if (have != null) return have;

		Node instance = Utils.getSingleREF(context);
		if (instance != null) {
			//change context to the-instance by REF
			context = instance;
			
			//search for have
			have = getByHave(pf, null, context, thes);
			if (have != null) return have;
		}
		
		Relationship prevTHE = null;
		
		//search 'IC' by 'IS' topology
		for (Relationship tdR : td_eval_ic.traverse(context).relationships()) {
			
			Statement st = Statements.relationshipType(tdR);
			if (st instanceof AN) {
				//System.out.println("GET IC -> IS "+tdR);
				if (prevTHE != null) {
					//search for have
					have = getByHave(pf, prevTHE, prevTHE.getEndNode(), thes);
					if (have != null) return have;
				}
				prevTHE = tdR;
				
			} else if (st instanceof SHALL) {
				//System.out.print("GET IC -> "+tdR);
				
				if (thes.contains(Utils.getSingleREF(tdR.getEndNode()))) {
					//System.out.println(" MATCH");
					
					//store
					final Node sNode = context;
					final Relationship r = tdR;

					return new Relationship[] {
						AnimoGraph.execute(new GraphOperation<Relationship>() {
							@Override
							public Relationship execute() {
								Relationship res = sNode.createRelationshipTo(r.getEndNode(), AN._);
								//RID.set(res, r.getId());
								return res;
							}
						})
					};
					
					//in-memory
					//Relationship res = new InMemoryRelationship(context, tdR.getEndNode(), AN._.relationshipType());
					//RID.set(res, tdR.getId());
					//return res;
					
					//as it
					//return tdR;
				}
				//System.out.println();
			}
		}
		
		if (prevTHE != null) {
			//search for have
			have = getByHave(pf, prevTHE, prevTHE.getEndNode(), thes);
			if (have != null) return have;
		}

		return null;
	}
	
	private Relationship[] getByHave(final PFlow pflow, Relationship op, final Node context, final Set<Node> thes) {
		if (context == null) return null;
		
		TraversalDescription trav = td.
				relationships(AN._, OUTGOING).
				relationships(REF._, OUTGOING).
				relationships(SHALL._, OUTGOING).
		evaluator(new Searcher(){
			@Override
			public Evaluation evaluate(Path path) {
				//System.out.println(path);
				return _evaluate_(path, thes);
			}
		});
		
		Map<Relationship, Path> paths = new FastMap<Relationship, Path>();

		for (Path path : trav.traverse(context)) {
			System.out.println("* "+path);
			
			if (path.length() == 1) {
				if (op == null) {
					System.out.println("WARNING: DONT KNOW OP");
					continue;
				}
				
				paths.put(op, path);
				//break;
			}
			
			Relationship fR = path.relationships().iterator().next();
			Path p = paths.get(fR);
			if (p == null || p.length() > path.length()) {
				paths.put(fR, path);
			}
		}
		
		int i = 0;
		
		Relationship[] res = new Relationship[paths.size()];
		for (Path path : paths.values()) {
			for (Relationship r : path.relationships()) {
				if (!pflow.isInStack(r)) {
					if (r.isType(AN._)) {
						if (Utils.haveContext(r.getEndNode())) {
							res[i] = r;
							//break;
						}
					} else if (r.isType(SHALL._)) {
						res[i] = r;
					}
				}
			}
			i++;
		}

//		while (true) {
//			try {
//				res = getDb().getRelationshipById(
//	                (Long)res.getProperty(RID.name())
//	            );
//			} catch (Exception e) {
//				break;
//			}
//		}
		
		return res;
	}
	
	private Relationship getShall(final Node context, final Set<Node> thes) {
//		TraversalDescription trav = td.
//		evaluator(new Searcher(){
//			@Override
//			public Evaluation evaluate(Path path) {
//				return _evaluate_(path, thes); //, IC._
//			}
//		});
//
//		Relationship res = null;
//		for (Path path : trav.traverse(context)) {
//			//TODO: check that this is only one answer
//			//System.out.println(path);
//			for (Relationship r : path.relationships()) {
//				res = r;
//				break;
//			}
//		}
		
		for (Relationship r : context.getRelationships(OUTGOING, SHALL._)) {
			for (QCAVector rr : Utils.getByREF(null, r)) {
				if (thes.contains(rr.getAnswer().getEndNode()))
					return r;
			}
		}
		
		return null;
	}
}
