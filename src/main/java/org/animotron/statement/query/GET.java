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
package org.animotron.statement.query;

import javolution.util.FastMap;
import javolution.util.FastSet;
import javolution.util.FastTable;

import org.animotron.Executor;
import org.animotron.graph.index.Order;
import org.animotron.io.Pipe;
import org.animotron.manipulator.Evaluator;
import org.animotron.manipulator.OnContext;
import org.animotron.manipulator.OnQuestion;
import org.animotron.manipulator.PFlow;
import org.animotron.manipulator.QCAVector;
import org.animotron.statement.Statement;
import org.animotron.statement.Statements;
import org.animotron.statement.operator.*;
import org.animotron.statement.relation.SHALL;
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
	
	private static boolean debug = false;
	
	private GET() { super("get", "<~"); }

    protected GET(String... name) {
        super(name);
    }

    public OnQuestion onCalcQuestion() {
		return question;
	}
	
	private OnQuestion question = new OnQuestion() {

		@Override
		public void act(final PFlow pf) {

			//if (debug) System.out.println("GET "+Thread.currentThread());

			final Relationship op = pf.getOP();
			
			final Node node = op.getEndNode();
			
			final FastSet<Relationship> visitedREFs = FastSet.newInstance();
			final FastSet<Node> thes = FastSet.newInstance(); 
			try {
				Relationship r = null;
				Pipe p = AN.getREFs(pf, pf.getVector());
				QCAVector theNode;
				while ((theNode = p.take()) != null) {
					r = theNode.getClosest();
					if (r.isType(AN._)) {
						try {
							for (QCAVector rr : Utils.eval(theNode)) {
								thes.add(rr.getClosest().getEndNode());
							}
						} catch (Exception e) {
							pf.sendException(e);
							return;
						}
					} else
						thes.add(r.getEndNode());
				}
	
				evalGet(pf, op, node, thes, visitedREFs);
			} finally {
				FastSet.recycle(thes);
				FastSet.recycle(visitedREFs);
			}
		}

		private void evalGet(
				final PFlow pf, 
				final Relationship op, 
				final Node node, 
				final Set<Node> thes, 
				final Set<Relationship> visitedREFs) {
			
			if (debug) { 
				Utils.debug(GET._, op, thes);
				System.out.println(pf.getVector());
			}

			//check, maybe, result was already calculated
			if (!Utils.results(pf)) {
				//no pre-calculated result, calculate it
				
				OnContext onContext = new OnContext(Executor.getFiber()) {
					@Override
					public void onMessage(QCAVector vector) {
						super.onMessage(vector);
						
						if (debug) System.out.println("GET on context "+Thread.currentThread());
						if (debug) System.out.println("GET ["+op+"] vector "+vector);
						
						if (vector == null) {
							if (cd.getCount() == 0)
								pf.done();
							return;
						}
						
						get(pf, vector, thes, visitedREFs);
					}
				};
				//pf.answerChannel().subscribe(onContext);
				
				if (Utils.haveContext(pf)) {
					Evaluator.sendQuestion(onContext, pf.getVector(), node);
				} else {
					
					if (debug) System.out.println("\nGET ["+op+"] empty ");

					FastSet<QCAVector> refs = FastSet.newInstance();
					try {
						QCAVector vector = pf.getVector();
						if (vector.getContext() != null) {
						
							for (QCAVector v : vector.getContext()) {
								refs.add(v);
							}
						}
						
//						vector = vector.getPrecedingSibling();
//						while (vector != null) {
//							refs.add(vector);
//							vector = vector.getPrecedingSibling();
//						}
						
						get(pf, refs, thes, visitedREFs);
					} finally {
						FastSet.recycle(refs);
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

	private boolean check(final PFlow pf, final QCAVector v, final Relationship toCheck, final Set<Node> thes, final Set<Relationship> visitedREFs) {
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
				
				if (debug) System.out.println("["+pf.getOP()+"] nextREFs ");//+Arrays.toString(nextREFs.toArray()));
	
				QCAVector v = null;
				for (FastSet.Record r = nextREFs.head(), end = nextREFs.tail(); (r = r.getNext()) != end;) {
					v = nextREFs.valueOf(r);
					
					if (debug) System.out.println("checking "+v);
					
					QCAVector next = v;
					while (next != null) {
						if (next.getQuestion() != null && next.hasAnswer())
							
							if (!check(pf, next, next.getUnrelaxedAnswer(), thes, visitedREFs)) {
								if (next.getAnswers() != null)
									for (QCAVector vv : next.getAnswers())
										if (check(pf, next, vv.getUnrelaxedAnswer(), thes, visitedREFs))
											found = true;
							} else {
								found = true;
							}
						
						visitedREFs.add(next.getQuestion());
						
						next = next.getPrecedingSibling();
					}
				}
				
				if (found) return true;
	
				//newREFs = new FastSet<QCAVector>();
	
				for (FastSet.Record r = nextREFs.head(), end = nextREFs.tail(); (r = r.getNext()) != end;) {
					v = nextREFs.valueOf(r);

					List<QCAVector> cs = v.getContext();
					if (cs != null) {
						for (QCAVector c : cs) {
							checkVector(c, newREFs, visitedREFs);
						}
					}
					
					QCAVector next = v;
					while (next != null) {
						t = next.getUnrelaxedAnswer();
						if (t != null && !t.equals(next.getQuestion())) {
							if (! t.isType(AN._))
								getOutgoingReferences(pf, next, t, t.getStartNode(), newREFs, visitedREFs);
							
							getOutgoingReferences(pf, next, t, t.getEndNode(), newREFs, visitedREFs);
						}

						//cs = next.getContext();
						//if (cs != null) {
						//	for (QCAVector c : cs) {
						//		checkVector(c, newREFs, visitedREFs);
						//	}
						//}
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
	
	private void checkVector(final QCAVector c, final Set<QCAVector> newREFs, final Set<Relationship> visitedREFs) {
		Relationship t = c.getUnrelaxedAnswer();
		if (t != null && !visitedREFs.contains(t))
			newREFs.add(c);
		else {
			t = c.getQuestion();
			if (!visitedREFs.contains(t))
				newREFs.add(c);
		}
	}
	
	private void getOutgoingReferences(PFlow pf, QCAVector vector, Relationship rr, Node node, Set<QCAVector> newREFs, Set<Relationship> visitedREFs) {

		QCAVector prev = null;
		IndexHits<Relationship> it = Order.context(node);
		try {
			for (Relationship r : it) {
				//if (debug) System.out.println(r);
				
				if (visitedREFs != null && visitedREFs.contains(r)) {
					continue;
				}
	
				prev = vector.question(r, prev); 

				Statement st = Statements.relationshipType(r);
				if (st instanceof AN) {
					//System.out.println(r);
					Pipe p = AN.getREFs(pf, prev);
					QCAVector v;
					while ((v = p.take()) != null) {
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
//					if (!pf.isInStack(r)) {
						try {
							//System.out.println("["+pf.getOP()+"] evaluate "+prev);
							Pipe in = Evaluator._.execute(prev);
							QCAVector v;
							while ((v = in.take()) != null) {
								prev.addAnswer(v);
								if (visitedREFs != null && !visitedREFs.contains(v.getAnswer()))
									newREFs.add(v);
							}
						} catch (IOException e) {
							// TODO Auto-generated catch block
							e.printStackTrace();
						}
//					}
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
			if (getByHave(pf, v, v.getQuestion(), ref.getStartNode(), thes))
				return true;
		}

		return false;
	}

	private boolean relaxReference(PFlow pf, QCAVector vector, Relationship op) {
		if (!op.isType(ANY._)) {
			if (debug) 
				System.out.println("["+pf.getOP()+"] answered "+op);
			
			pf.sendAnswer(pf.getVector().answered(op, vector), RESULT);
			return true;
		}
		
		if (debug) 
			System.out.println("["+pf.getOP()+"] Relaxing "+op+" @ "+vector);
		
		try {
			Pipe in = Evaluator._.execute(vector.question(op));

			//if (!in.hasNext()) return false;
			
			boolean answered = false;
			
			Relationship res = null;
			QCAVector v;
			while ((v = in.take()) != null) {
				res = v.getAnswer();
				if (!pf.isInStack(res)) {
					
					if (debug) 
						System.out.println("["+pf.getOP()+"] Relaxing answered "+v.getAnswer());
					
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
		
		FastMap<Relationship, Path> paths = FastMap.newInstance();
		try {
	
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
			FastTable<Relationship> resByHAVE = FastTable.newInstance();
			FastTable<Relationship> resByIS = FastTable.newInstance();
			try {
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

							} else if (r.isType(REF._) && path.length() == 1) {
								res = op;
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
			} finally{
				FastTable.recycle(resByHAVE);
				FastTable.recycle(resByIS);
			}
			return true;
		} finally {
			FastMap.recycle(paths);
		}
	}
}
