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
import javolution.util.FastMap;
import javolution.util.FastSet;
import org.animotron.Executor;
import org.animotron.graph.AnimoGraph;
import org.animotron.graph.GraphOperation;
import org.animotron.graph.index.Order;
import org.animotron.io.PipedInput;
import org.animotron.io.PipedOutput;
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

import com.sun.org.apache.regexp.internal.RE;

import java.io.IOException;
import java.util.List;
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
public class GET extends AbstractQuery implements Shift {

	public static final GET _ = new GET();
	
	private static boolean debug = false;
	
	private GET() { super("get", "<~"); }

    protected GET(String... name) {
        super(name);
    }

	static TraversalDescription td = Traversal.description().
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
			
			Relationship r = null;
			for (QCAVector theNode : AN.getREFs(pf, op)) {
				r = theNode.getAnswer();
				if (r.isType(AN._)) {
					try {
						for (QCAVector rr : Utils.eval(pf, theNode)) {
							thes.add(rr.getAnswer().getEndNode());
						}
					} catch (IOException e) {
						pf.sendException(e);
						return;
					}
				} else
					thes.add(r.getEndNode());
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
			
			//Utils.debug(GET._, op, thes);

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
						
						get(pf, op, vector, thes, visitedREFs);
					}

					@Override
					public DisposingExecutor getQueue() {
						return Executor.getFiber();
					}
				};
				pf.answerChannel().subscribe(onContext);
				
				if (Utils.haveContext(pf)) {
					super.onMessage(pf);
				} else {
					
					if (debug) System.out.println("GET ["+op+"] empty ");

					boolean first = true;
					boolean rSet = false;
					for (QCAVector vector : pf.getPFlowPath()) {
						//System.out.println("CHECK PFLOW "+vector);
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
						if (rSet) break;
					}
				}
			}
		};

	};
	
	public boolean get(PFlow pf, Relationship op, QCAVector vector, final Set<Node> thes, Set<Relationship> visitedREFs) {
		FastSet<QCAVector> refs = FastSet.newInstance();
		try {
			refs.add(vector);
			
			return get(pf, op, refs, thes, visitedREFs);
		} finally {
			FastSet.recycle(refs);
		}
	}

	public boolean get(final PFlow pf, Relationship op, Node ref, final Set<Node> thes, final Set<Relationship> visitedREFs) {
		if (searchForHAVE(pf, op, ref, thes))
			return true;
//				if (!pf.isInStack(have[i]))
//					set.add(new QCAVector(pf.getOP(), have[i]));
		
		//if (!set.isEmpty()) return set;

		FastSet<QCAVector> newREFs = FastSet.newInstance();
		try {
			getOutgoingReferences(pf, pf.getVector(), null, ref, newREFs, null);
			
			return get(pf, op, newREFs, thes, visitedREFs);
		} finally {
			FastSet.recycle(newREFs);
		}
	}
	
	private boolean check(final PFlow pf, final Relationship op, final QCAVector v, final Relationship toCheck, final Set<Node> thes, Set<Relationship> visitedREFs) {
		if (toCheck == null) return false;
		
		visitedREFs.add(toCheck);
		
		if (searchForHAVE(pf, toCheck, v, thes))
			return true;

		//if (!pf.isInStack(have[i])) {
		//set.add(new QCAVector(op, v, have[i]));

		return false;
	}

	public boolean get(
			final PFlow pf,
			final Relationship op,
			final Set<QCAVector> REFs, 
			final Set<Node> thes, 
			Set<Relationship> visitedREFs) {
		
		//System.out.println("GET context = "+ref);
		
		if (visitedREFs == null) visitedREFs = new FastSet<Relationship>();
		
		FastSet<QCAVector> nextREFs = FastSet.newInstance();
		FastSet<QCAVector> newREFs = FastSet.newInstance();
		
		FastSet<QCAVector> tmp = null;

		try {
			nextREFs.addAll(REFs);
	
			boolean found = false;
			
			Relationship t = null;
			
			while (true) {
				
				if (debug) System.out.println("nextREFs ");//+Arrays.toString(nextREFs.toArray()));
	
				for (QCAVector v : nextREFs) {
					if (debug) System.out.println("checking "+v);
					
					QCAVector next = v;
					while (next != null) {
						if (!check(pf, op, v, v.getUnrelaxedAnswer(), thes, visitedREFs)) {
							found = found || check(pf, op, v, v.getQuestion(), thes, visitedREFs);
						} else {
							found = true;
						}
						next = next.getPrecedingSibling();
					}
	
				}
				
				if (found) return true;
	
				//newREFs = new FastSet<QCAVector>();
	
				for (QCAVector vector : nextREFs) {
					List<QCAVector> cs = vector.getContext();
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
					
					QCAVector next = vector;
					while (next != null) {
						t = next.getUnrelaxedAnswer();
						if (t != null) {
							if (! t.isType(AN._))
								getOutgoingReferences(pf, next, t, t.getStartNode(), newREFs, visitedREFs);
							
							getOutgoingReferences(pf, next, t, t.getEndNode(), newREFs, visitedREFs);
						}
						next = next.getPrecedingSibling();
					}
				}
	
				if (newREFs.size() == 0) return false;
				
				//swap
				tmp = nextREFs; 
				nextREFs = newREFs;
				newREFs = tmp;
				newREFs.clear();
			}
		} finally {
			FastSet.recycle(nextREFs);
			FastSet.recycle(newREFs);
		}
	}
	
	private void getOutgoingReferences(PFlow pf, QCAVector vector, Relationship rr, Node node, Set<QCAVector> newREFs, Set<Relationship> visitedREFs) {

		QCAVector prev = null;
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
	
				prev = vector.question(r, prev); 

				Statement st = Statements.relationshipType(r);
				if (st instanceof AN) {
					//System.out.println(r);
					for (QCAVector v : AN.getREFs(pf, r)) {
						Relationship t = v.getAnswer();
						
						prev.addAnswer(v);
						
						//System.out.println(t);
						if (visitedREFs != null && !visitedREFs.contains(t)) {
							v.setPrecedingSibling(prev);
							prev = v;
							newREFs.add(v);
						}
					}
	
				} else if (st instanceof Reference) {
					if (!pf.isInStack(r)) {
						try {
							PipedInput<QCAVector> in = Evaluator._.execute(new PFlow(pf), prev);
							
							for (QCAVector v : in) {
								prev.addAnswer(v);
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
	
	private boolean searchForHAVE(
			final PFlow pf, 
			final Relationship ref,
			final QCAVector v,
			final Set<Node> thes) {
		
		if (ref.isType(REF._) && thes.contains(ref.getEndNode())) {
			if (!pf.isInStack(ref)) {
				pf.sendAnswer(v.answered(ref));
				return true;
			}
			return false;
		}
		
		boolean checkStart = true;
		if (ref.isType(AN._)) {
			checkStart = false;
		}
		
		//search for inside 'HAVE'
		//return searchForHAVE(pf, ref, ref.getEndNode(), thes);
		if (getByHave(pf, v, ref, ref.getEndNode(), thes))
			return true;

		//search for local 'HAVE'
		if (checkStart) {
			if (getByHave(pf, v, null, ref.getStartNode(), thes))
				return true;
		}

		return false;
	}

	private boolean searchForHAVE(final PFlow pflow, Relationship op, final Node ref, final Set<Node> thes) {
		
		//search for inside 'HAVE'
		if (getByHave(pflow, pflow.getVector(), op, ref, thes))
			return true;

		//search 'IC' by 'IS' topology
		for (Relationship tdR : Utils.td_eval_IS.traverse(ref).relationships()) {

			//System.out.println("GET IC -> IS "+tdR);
			
			Relationship r = getShall(tdR.getEndNode(), thes);
			if (r != null) {
				final Node sNode = ref;
				final Node eNode = r.getEndNode();
				final long id = r.getId();
				Relationship res = 
					AnimoGraph.execute(new GraphOperation<Relationship>() {
						@Override
						public Relationship execute() {
							Relationship res = sNode.createRelationshipTo(eNode, AN._);
							RID.set(res, id);
							return res;
						}
					});
				
				pflow.sendAnswer(pflow.getVector().answered(res));
			}
			
			//search for have
			if (getByHave(pflow, pflow.getVector(), tdR, tdR.getEndNode(), thes))
				return true;
		}
		
		return false;
	}
	
	//XXX: in-use by SELF
	public boolean getBySELF(final PFlow pf, Node context, final Set<Node> thes) {
		
		//System.out.println("GET get context = "+context);

		//search for local 'HAVE'
		if (getByHave(pf, pf.getVector(), null, context, thes))
			return true;

		Node instance = Utils.getSingleREF(context);
		if (instance != null) {
			//change context to the-instance by REF
			context = instance;
			
			//search for have
			if (getByHave(pf, pf.getVector(), null, context, thes))
				return true;
		}
		
		Relationship prevTHE = null;
		
		//search 'IC' by 'IS' topology
		for (Relationship tdR : td_eval_ic.traverse(context).relationships()) {
			
			Statement st = Statements.relationshipType(tdR);
			if (st instanceof AN) {
				//System.out.println("GET IC -> IS "+tdR);
				if (prevTHE != null) {
					//search for have
					if (getByHave(pf, pf.getVector(), prevTHE, prevTHE.getEndNode(), thes))
						return true;
				}
				prevTHE = tdR;
				
			} else if (st instanceof SHALL) {
				//System.out.print("GET IC -> "+tdR);
				
				if (thes.contains(Utils.getSingleREF(tdR.getEndNode()))) {
					//System.out.println(" MATCH");
					
					//store
					final Node sNode = context;
					final Relationship r = tdR;

					Relationship res = 
						AnimoGraph.execute(new GraphOperation<Relationship>() {
							@Override
							public Relationship execute() {
								Relationship res = sNode.createRelationshipTo(r.getEndNode(), AN._);
								//RID.set(res, r.getId());
								return res;
							}
						});
					pf.sendAnswer(pf.getVector().answered(res), RESULT);
					return true;
					
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
			if (getByHave(pf, pf.getVector(), prevTHE, prevTHE.getEndNode(), thes))
				return true;
		}

		return false;
	}
	
	private boolean relaxReference(PFlow pf, QCAVector vector, Relationship op) {
		if (!op.isType(ANY._)) {
			pf.sendAnswer(vector.answered(op), RESULT);
			return true;
		}
		
		System.out.println("Relaxing ....");
		try {
			PipedInput<QCAVector> in = Evaluator._.execute(new PFlow(pf), vector.question(op));

			if (!in.hasNext()) return false;
			
			boolean answered = false;
			
			Relationship res = null;
			for (QCAVector v : in) {
				res = v.getAnswer();
				if (!pf.isInStack(res)) {
					pf.sendAnswer(v, RESULT);
					answered = true;
				}
			}
			return answered;
			
		} catch (IOException e) {
			pf.sendException(e);
		}
		return false;
	}

	private static TraversalDescription prepared = td.
			relationships(ANY._, OUTGOING).
			relationships(AN._, OUTGOING).
			relationships(REF._, OUTGOING).
			relationships(SHALL._, OUTGOING);
	
	private boolean getByHave(final PFlow pf, QCAVector vector, Relationship op, final Node context, final Set<Node> thes) {
		if (context == null) return false;
		
		TraversalDescription trav = prepared.
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
			
			if (path.length() == 2) {
				//UNDERSTAND: should we check context
				if (relaxReference(pf, vector, path.relationships().iterator().next()))
					return true;
			}

			Relationship fR = path.relationships().iterator().next();
			Path p = paths.get(fR);
			if (p == null || p.length() > path.length()) {
				paths.put(fR, path);
			}
		}
		
		Relationship startBy = null;
		
		Relationship res = null;
		List<Relationship> resByHAVE = new FastList<Relationship>();
		List<Relationship> resByIS = new FastList<Relationship>();

		for (Path path : paths.values()) {
			for (Relationship r : path.relationships()) {
				if (startBy == null)
					startBy = r;
					
				if (!pf.isInStack(r)) {
					if (r.isType(AN._)) {
						if (Utils.haveContext(r.getEndNode())) {
							res = r;
							//break;
						} else if (res == null && (startBy.isType(REF._) || (op != null && (op.isType(REF._) || op.isType(RESULT))))) {
							res = r;
							//break;
						}
					} else if (r.isType(SHALL._)) {
						res = r;
						//break;
					}
				}
			}
			if (res != null) {
				if (startBy != null && startBy.isType(REF._))
					resByIS.add(res);
				else
					resByHAVE.add(res);
			}
			startBy = null;
		}

		if (!resByHAVE.isEmpty()) {
			for (Relationship r : resByHAVE) {
				pf.sendAnswer(pf.getVector().answered(r));//XXX: add context?
			}
		} else {
			if (resByIS.isEmpty())
				return false;
	
			for (Relationship r : resByIS) {
				pf.sendAnswer(pf.getVector().answered(r));//XXX: add context?
			}
		}
		return true;
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
