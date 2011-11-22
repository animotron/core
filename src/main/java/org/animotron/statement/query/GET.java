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

import javolution.util.FastSet;
import org.animotron.Executor;
import org.animotron.graph.AnimoGraph;
import org.animotron.graph.GraphOperation;
import org.animotron.graph.index.Order;
import org.animotron.io.PipedInput;
import org.animotron.manipulator.QCAVector;
import org.animotron.manipulator.Evaluator;
import org.animotron.manipulator.OnQuestion;
import org.animotron.manipulator.PFlow;
import org.animotron.statement.Statement;
import org.animotron.statement.Statements;
import org.animotron.statement.operator.*;
import org.animotron.statement.relation.HAVE;
import org.animotron.statement.relation.IC;
import org.animotron.statement.relation.IS;
import org.jetlang.channels.Subscribable;
import org.jetlang.core.DisposingExecutor;
import org.neo4j.graphdb.*;
import org.neo4j.graphdb.index.IndexHits;
import org.neo4j.graphdb.traversal.Evaluation;
import org.neo4j.graphdb.traversal.TraversalDescription;
import org.neo4j.kernel.Traversal;
import org.neo4j.kernel.Uniqueness;

import java.io.IOException;
import java.util.List;
import java.util.Set;

import static org.animotron.Properties.RID;
import static org.animotron.graph.AnimoGraph.getDb;
import static org.animotron.graph.RelationshipTypes.*;
import static org.neo4j.graphdb.Direction.*;
import static org.neo4j.graphdb.traversal.Evaluation.*;

/**
 * Query operator 'Get'. Return 'have' relations on provided context.
 * 
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 */
public class GET extends AbstractQuery implements Evaluable, Query {

	public static final GET _ = new GET();
	
	private GET() { super("get"); }

	TraversalDescription td = Traversal.description().
			depthFirst().uniqueness(Uniqueness.RELATIONSHIP_PATH);

	private static TraversalDescription td_eval_ic = 
			Traversal.description().
				breadthFirst().
				relationships(IS._, OUTGOING).
				relationships(IC._, OUTGOING);

	public OnQuestion onCalcQuestion() {
		return question;
	}
	
	private OnQuestion question = new OnQuestion() {

		@Override
		public void onMessage(final PFlow pf) {
			final Relationship op = pf.getOP();
			
			final Node node = op.getEndNode();
			
			final List<Relationship> suffixes = Utils.getSuffixes(node);
			
			final Set<Relationship> visitedREFs = new FastSet<Relationship>();

			for (QCAVector theNode : AN.getREFs(pf, op)) {
				evalGet(pf, op, node, theNode.getAnswer().getEndNode(), suffixes, visitedREFs);
			}
			
			pf.await();
			pf.done();
		}

		private void evalGet(
				final PFlow pf, 
				final Relationship op, 
				final Node node, 
				final Node theNode, 
				final List<Relationship> suffixes, 
				final Set<Relationship> visitedREFs) {
			
			System.out.print("GET '");
			try {
				System.out.print(name(theNode));
			} catch (Exception e) {}
			System.out.println("' ["+theNode+"]");
			//System.out.println(pf.getPathHash()[0]+" "+pf.getPFlowPath());
			
			//for (Relationship r : pf.getPFlowPath()) {
			//	System.out.println(""+r+" "+r.getType());
			//}

			//check, maybe, result was already calculated
			if (!Utils.results(pf)) {
				//no pre-calculated result, calculate it
				
				Subscribable<QCAVector> onContext = new Subscribable<QCAVector>() {
					@Override
					public void onMessage(QCAVector vector) {
						System.out.println("GET ["+theNode+"] vector "+vector);
						
						if (vector == null) {
							pf.countDown();
							return;
						}

						//final Relationship have = searchForHAVE(context, name);
						final Set<QCAVector> rSet = get(pf, op, vector, theNode, suffixes, visitedREFs);
						if (rSet != null) {
							for (QCAVector v : rSet) {
								pf.sendAnswer(v, HAVE._);
							}
							return;
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
					
					for (QCAVector vector : pf.getPFlowPath()) {
						//System.out.println("CHECK PFLOW "+vector);
						Set<QCAVector> rSet = get(pf, op, vector, theNode, suffixes, visitedREFs);
						if (rSet != null) {
							for (QCAVector v : rSet) {
								pf.sendAnswer(v, HAVE._);
							}
							break;
						}
					}
				}
			}
		};

	};
	
	public Set<QCAVector> get(PFlow pf, Relationship op, QCAVector vector, final Node theNode, final List<Relationship> suffixes, Set<Relationship> visitedREFs) {
		Set<QCAVector> refs = new FastSet<QCAVector>();
		refs.add(vector);
		
		return get(pf, op, refs, theNode, suffixes, visitedREFs); 
	}

	public Set<QCAVector> get(final PFlow pf, Relationship op, Node ref, final Node theNode, final List<Relationship> suffixes, final Set<Relationship> visitedREFs) {
		Set<QCAVector> set = new FastSet<QCAVector>();
		
		Relationship have = searchForHAVE(pf, ref, theNode, suffixes);
		if (have != null && !pf.isInStack(have)) 
			set.add(new QCAVector(pf.getOP(), have));
		
		if (!set.isEmpty()) return set;

		Set<QCAVector> newREFs = new FastSet<QCAVector>();
		getOutgoingReferences(pf, ref, newREFs, null);
		
		return get(pf, op, newREFs, theNode, suffixes, visitedREFs); 
	}

	public Set<QCAVector> get(
			final PFlow pf,
			final Relationship op,
			final Set<QCAVector> REFs, 
			final Node theNode, 
			final List<Relationship> suffixes, 
			Set<Relationship> visitedREFs) {
		
		//System.out.println("GET context = "+ref);
		
		if (visitedREFs == null) visitedREFs = new FastSet<Relationship>();
		
		Set<QCAVector> set = new FastSet<QCAVector>();
		
		Set<QCAVector> nextREFs = new FastSet<QCAVector>();
		nextREFs.addAll(REFs);

		Relationship have = null;
		
		boolean first = true;
		
		while (true) {
			
			//System.out.println("nextREFs "+Arrays.toString(nextREFs.toArray()));

			for (QCAVector v : nextREFs) {
				//System.out.println("checking "+n);
				have = searchForHAVE(pf, v.getUnrelaxedClosest(), theNode, suffixes);
				if (have != null && !pf.isInStack(have)) { 
					set.add(new QCAVector(op, v, have));
					//System.out.println("FOUND");
				}
				visitedREFs.add(v.getUnrelaxedClosest());
			}
			
			if (set.size() > 0) return set;

			Set<QCAVector> newREFs = new FastSet<QCAVector>();

			for (QCAVector vector : nextREFs) {
				QCAVector c = vector.getContext();
				if (c != null) {
					Relationship t = c.getUnrelaxedClosest();
					if (!visitedREFs.contains(t))
						newREFs.add(c);
				}
				
				
				Relationship n = vector.getClosest();
				//System.out.println(""+n);
				//System.out.println("getStartNode OUTGOING");
				if (first || !REFs.contains(n)) {
					IndexHits<Relationship> it = Order.queryDown(n.getStartNode());
					try {
						for (Relationship r : it) {
							if (r.equals(n)) continue;
							//System.out.println(r);
							
							Statement st = Statements.relationshipType(r);
							if (st instanceof AN) {
								for (QCAVector v : AN.getREFs(pf, r)) {
									Relationship t = v.getAnswer();
									if (!visitedREFs.contains(t))
										newREFs.add(v);
								}
							} else if (st instanceof Reference) {
								try {
									if (!pf.isInStack(r)) {
										PipedInput<QCAVector> in = Evaluator._.execute(new PFlow(pf), r);
										
										for (QCAVector rr : in) {
											if (!visitedREFs.contains(rr.getAnswer()))
												newREFs.add(rr);
										}
									}
								} catch (IOException e) {
									// TODO Auto-generated catch block
									e.printStackTrace();
								}
							}
						}
					} finally {
						it.close();
					}
				}
				first = false;

				//System.out.println("getEndNode OUTGOING");
				getOutgoingReferences(pf, n.getEndNode(), newREFs, visitedREFs);
			}

			if (newREFs.size() == 0) return null;
			
			nextREFs = newREFs;
		}
	}
	
	private void getOutgoingReferences(PFlow pf, Node node, Set<QCAVector> newREFs, Set<Relationship> visitedREFs) {

		IndexHits<Relationship> it = Order.queryDown(node);
		try {
			for (Relationship r : it) {
				//System.out.println(r);
	
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
							
							for (QCAVector rr : in) {
								if (visitedREFs != null && !visitedREFs.contains(rr.getAnswer()))
									newREFs.add(rr);
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
	
	private Relationship searchForHAVE(
			final PFlow pf, 
			final Relationship ref, 
			final Node theNode, 
			final List<Relationship> suffixes) {
		
		boolean checkStart = true;
		if (ref.isType(RESULT) || !(ref.isType(REF) || ref.isType(org.animotron.statement.operator.REF._))) {
			//System.out.print("WRONG WRONG WRONG WRONG ref = "+ref+" - "+ref.getType());
//			try {
//				System.out.print(" "+reference(ref));
//			} catch (Exception e) {
//			} finally {
//				System.out.println();
//			}
			checkStart = false;
		}
		
		Relationship have = null;
		
		//search for local 'HAVE'
		if (checkStart) {
			have = getByHave(pf, ref.getStartNode(), theNode, suffixes);
			if (have != null) return have;
		}

		//search for inside 'HAVE'
		return searchForHAVE(pf, ref.getEndNode(), theNode, suffixes);
	}

	private Relationship searchForHAVE(final PFlow pflow, final Node ref, final Node theNode, final List<Relationship> suffixes) {
		
		Relationship have = null;

		//search for inside 'HAVE'
		have = getByHave(pflow, ref, theNode, suffixes);
		if (have != null) {
			have = checkSuffixes(suffixes, have);
			if (have != null) return have;
		}

		//search 'IC' by 'IS' topology
		for (Relationship tdR : Utils.td_eval_IS.traverse(ref).relationships()) {

			//System.out.println("GET IC -> IS "+tdR);
			
			Relationship r = getByIC(tdR.getEndNode(), theNode, suffixes);
			if (r != null) {
				final Node sNode = ref;
				final Node eNode = r.getEndNode();
				final long id = r.getId();
				return AnimoGraph.execute(new GraphOperation<Relationship>() {
					@Override
					public Relationship execute() {
						Relationship res = sNode.createRelationshipTo(eNode, HAVE._);
						RID.set(res, id);
						return res;
					}
				});
			}
			
			//search for have
			have = getByHave(pflow, tdR.getEndNode(), theNode, suffixes);
			if (have != null) return have;
		}
		
		return null;
	}
	
	private Relationship checkSuffixes(final List<Relationship> suffixes, final Relationship res) {

		if (suffixes == null || res == null) return res;
		
		//System.out.println("checkSuffixes checkSuffixes checkSuffixes "+res);
		
		Node node = res.getStartNode();
		
		for (Relationship r : Utils.td_eval_IS.traverse(node).relationships()) {
			//System.out.println(r);
			for (Relationship rr : suffixes) {
				if (r.getEndNode().equals(rr.getEndNode()))
					return res;
			}
		}
		return null;
	}

	//XXX: in-use by SELF
	public Relationship getBySELF(final PFlow pf, Node context, final Node theNode) {
		
		//System.out.println("GET get context = "+context);

		//search for local 'HAVE'
		Relationship have = getByHave(pf, context, theNode, null);
		if (have != null) return have;

		Node instance = Utils.getSingleREF(context);
		if (instance != null) {
			//change context to the-instance by REF
			context = instance;
			
			//search for have
			have = getByHave(pf, context, theNode, null);
			if (have != null) return have;
		}
		
		Relationship prevTHE = null;
		
		//search 'IC' by 'IS' topology
		for (Relationship tdR : td_eval_ic.traverse(context).relationships()) {
			
			Statement st = Statements.relationshipType(tdR);
			if (st instanceof IS) {
				//System.out.println("GET IC -> IS "+tdR);
				if (prevTHE != null) {
					//search for have
					have = getByHave(pf, prevTHE.getEndNode(), theNode, null);
					if (have != null) return have;
				}
				prevTHE = tdR;
				
			} else if (st instanceof IC) {
				//System.out.print("GET IC -> "+tdR);
				
				if (theNode.equals(Utils.getSingleREF(tdR.getEndNode()))) {
					//System.out.println(" MATCH");
					
					//store
					final Node sNode = context;
					final Relationship r = tdR;

					return AnimoGraph.execute(new GraphOperation<Relationship>() {
						@Override
						public Relationship execute() {
							Relationship res = sNode.createRelationshipTo(r.getEndNode(), HAVE._);
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
				//System.out.println();
			}
		}
		
		if (prevTHE != null) {
			//search for have
			have = getByHave(pf, prevTHE.getEndNode(), theNode, null);
			if (have != null) return have;
		}

		return null;
	}
	
	private Relationship getByHave(final PFlow pflow, final Node context, final Node theNode, final List<Relationship> suffixes) {
		TraversalDescription trav = td.
		evaluator(new Searcher(){
			@Override
			public Evaluation evaluate(Path path) {
				return _evaluate_(path, theNode, HAVE._);
			}
		});

		Relationship res = null;
		for (Path path : trav.traverse(context)) {
			//TODO: check that this is only one answer
			//System.out.println(path);
			for (Relationship r : path.relationships()) {
				if (!pflow.isInStack(r)) {
					if (r.isType(HAVE._)) {
						res = r;
						break;
					}
				}
			}
		}
		
		while (true) {
			try {
				res = getDb().getRelationshipById(
	                (Long)res.getProperty(RID.name())
	            );
			} catch (Exception e) {
				break;
			}
		}
		
		return checkSuffixes(suffixes, res);
	}
	
	private Relationship getByIC(final Node context, final Node theNode, final List<Relationship> suffixes) {
		TraversalDescription trav = td.
		evaluator(new Searcher(){
			@Override
			public Evaluation evaluate(Path path) {
				return _evaluate_(path, theNode, IC._);
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
		
		return checkSuffixes(suffixes, res);
	}

	abstract class Searcher implements org.neo4j.graphdb.traversal.Evaluator {

		public Evaluation _evaluate_(Path path, Node target, RelationshipType type) {
			//System.out.println(path);
			
			if (path.length() == 0)
				return EXCLUDE_AND_CONTINUE;
			
			Relationship r = path.lastRelationship(); 

			if (path.length() == 1) {
				if (r.isType(type))
					return EXCLUDE_AND_CONTINUE;
					
				return EXCLUDE_AND_PRUNE;
				
			} else if (path.length() >= 2) {
				if (r.isType(org.animotron.statement.operator.REF._)) {
					Node node = r.getEndNode();
					if (target.equals(node)) 
						return INCLUDE_AND_PRUNE;

					return EXCLUDE_AND_CONTINUE;
				
				//XXX: check direction!
				} else if (r.isType(IS._)) {
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
