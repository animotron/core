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
import org.animotron.graph.index.Order;
import org.animotron.io.PipedInput;
import org.animotron.manipulator.Evaluator;
import org.animotron.manipulator.OnContext;
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
import java.util.List;
import java.util.Map;
import java.util.Set;

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
	
	private static boolean debug = true;
	
	private GET() { super("get", "<~"); }

    protected GET(String... name) {
        super(name);
    }

    public OnQuestion onCalcQuestion() {
		return question;
	}
	
	private OnQuestion question = new OnQuestion() {

		@Override
		public void onMessage(final PFlow pf) {

			System.out.println("GET "+Thread.currentThread());

			final Relationship op = pf.getOP();
			
			final Node node = op.getEndNode();
			
			final Set<Relationship> visitedREFs = new FastSet<Relationship>();

			final Set<Node> thes = new FastSet<Node>(); 
			
			Relationship r = null;
			for (QCAVector theNode : AN.getREFs(pf, pf.getVector())) {
				r = theNode.getClosest();
				if (r.isType(AN._)) {
					try {
						for (QCAVector rr : Utils.eval(theNode)) {
							thes.add(rr.getClosest().getEndNode());
						}
					} catch (IOException e) {
						pf.sendException(e);
						return;
					}
				} else
					thes.add(r.getEndNode());
			}

			evalGet(pf, op, node, thes, visitedREFs);
			
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
				
				OnContext onContext = new OnContext(Executor.getFiber()) {
					@Override
					public void onMessage(QCAVector vector) {
						super.onMessage(vector);
						
						System.out.println("GET on context "+Thread.currentThread());
						if (debug) System.out.println("GET ["+op+"] vector "+vector);
						
						if (vector == null) {
							pf.countDown();
							return;
						}
						
						get(pf, vector, thes, visitedREFs);
					}
				};
				//pf.answerChannel().subscribe(onContext);
				
				if (Utils.haveContext(pf)) {
					Evaluator.sendQuestion(onContext, pf.getVector(), node);
					//super.onMessage(pf);
				} else {
					
					if (debug) System.out.println("\nGET ["+op+"] empty ");

					QCAVector vector = pf.getVector();
					if (vector.getContext() != null) {
					
						Set<QCAVector> refs = new FastSet<QCAVector>();
						for (QCAVector v : vector.getContext()) {
							refs.add(v);
						}
						get(pf, refs, thes, visitedREFs);
					}
				}
			}
		};

	};
	
	public boolean get(PFlow pf, QCAVector vector, final Set<Node> thes, Set<Relationship> visitedREFs) {
		FastSet<QCAVector> refs = FastSet.newInstance();
		try {
			refs.add(vector);
			
			return get(pf, refs, thes, visitedREFs);
		} finally {
			FastSet.recycle(refs);
		}
	}

	private boolean check(final PFlow pf, final QCAVector v, final Relationship toCheck, final Set<Node> thes, Set<Relationship> visitedREFs) {
		if (toCheck == null) return false;
		
		if (visitedREFs.contains(toCheck)) return false;
		//if (toCheck.isType(REF._) && visitedREFs.contains(v.getQuestion())) return false;
		
		visitedREFs.add(toCheck);
		
		if (searchForHAVE(pf, toCheck, v, thes))
			return true;

		//if (!pf.isInStack(have[i])) {
		//set.add(new QCAVector(op, v, have[i]));

		return false;
	}

	public boolean get(
			final PFlow pf,
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
						if (!check(pf, next, next.getUnrelaxedAnswer(), thes, visitedREFs)) {
							//if (debug) System.out.println("checking question");
							found = found || check(pf, next, next.getQuestion(), thes, visitedREFs);
						} else {
							visitedREFs.add(next.getQuestion());
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
		IndexHits<Relationship> it = Order.context(node);
		try {
			for (Relationship r : it) {
				//if (debug) System.out.println(r);
				
				if (visitedREFs != null && !visitedREFs.contains(r)) {
					continue;
				}
	
				prev = vector.question(r, prev); 

				Statement st = Statements.relationshipType(r);
				if (st instanceof AN) {
					//System.out.println(r);
					for (QCAVector v : AN.getREFs(pf, prev)) {
						Relationship t = v.getClosest();
						
						prev.addAnswer(v);
						
						//System.out.println(t);
						if (visitedREFs != null && !visitedREFs.contains(t)) {
							//v.setPrecedingSibling(prev);
							//prev = v;
							newREFs.add(v);
						}
					}
	
				} else if (st instanceof Reference) {
					if (!pf.isInStack(r)) {
						try {
							PipedInput<QCAVector> in = Evaluator._.execute(prev);
							
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
		
//		if ((ref.isType(REF._) || ref.isType(THE._)) && thes.contains(ref.getEndNode())) {
//			if (!pf.isInStack(ref)) {
//				pf.sendAnswer(pf.getVector().answered(ref, v), AN._);
//				return true;
//			}
//			return false;
//		}
		
		//search for inside 'HAVE'
		//return searchForHAVE(pf, ref, ref.getEndNode(), thes);
		if (getByHave(pf, v, ref, ref.getEndNode(), thes))
			return true;

		//search for local 'HAVE'
		if (ref.isType(REF._)) {
			if (getByHave(pf, v, null, ref.getStartNode(), thes))
				return true;
		}

		return false;
	}

	private boolean relaxReference(PFlow pf, QCAVector vector, Relationship op) {
		if (!op.isType(ANY._)) {
			if (debug) System.out.println("["+pf.getOP()+"] answered "+op);
			pf.sendAnswer(pf.getVector().answered(op, vector), RESULT);
			return true;
		}
		
		if (debug) System.out.println("["+pf.getOP()+"] Relaxing "+op+" @ "+vector);
		try {
			PipedInput<QCAVector> in = Evaluator._.execute(vector.question(op));

			if (!in.hasNext()) return false;
			
			boolean answered = false;
			
			Relationship res = null;
			for (QCAVector v : in) {
				res = v.getAnswer();
				if (!pf.isInStack(res)) {
					if (debug) System.out.println("["+pf.getOP()+"] answered "+v.getAnswer());
					pf.sendAnswer(vector.answered(v.getAnswer(), v), RESULT);
					answered = true;
				}
			}
			return answered;
			
		} catch (IOException e) {
			pf.sendException(e);
		}
		return false;
	}

	private static TraversalDescription prepared = 
			Traversal.description().
			depthFirst().
			uniqueness(Uniqueness.RELATIONSHIP_PATH).
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
			if (debug) System.out.println("["+pf.getOP()+"] * "+path);
			
			if (path.length() == 1) {
				if (op == null) {
					System.out.println("WARNING: DONT KNOW OP");
					continue;
				}
				
				if (pf.getOP().equals(op))
					continue;
				
				paths.put(op, path);
				continue;
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
		
		if (paths.isEmpty()) return false;
		
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
					} else if (r.isType(ANY._)) {
						res = r;

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
				relaxReference(pf, vector, r);
			}
		} else {
			if (resByIS.isEmpty())
				return false;
	
			for (Relationship r : resByIS) {
				relaxReference(pf, vector, r);
			}
		}
		return true;
	}
}
