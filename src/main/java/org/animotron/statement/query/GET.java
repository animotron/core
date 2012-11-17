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

import javolution.util.FastList;
import javolution.util.FastMap;
import javolution.util.FastSet;
import javolution.util.FastTable;
import org.animotron.graph.Properties;
import org.animotron.graph.index.AShift;
import org.animotron.graph.index.IteratorSingle;
import org.animotron.graph.index.Order;
import org.animotron.io.Pipe;
import org.animotron.manipulator.*;
import org.animotron.statement.Statement;
import org.animotron.statement.Statements;
import org.animotron.statement.operator.*;
import org.animotron.statement.relation.SHALL;
import org.animotron.statement.value.VALUE;
import org.neo4j.graphdb.*;
import org.neo4j.graphdb.index.IndexHits;
import org.neo4j.graphdb.traversal.BranchState;
import org.neo4j.graphdb.traversal.Evaluation;
import org.neo4j.graphdb.traversal.InitialStateFactory;
import org.neo4j.graphdb.traversal.TraversalDescription;
import org.neo4j.kernel.Traversal;
import org.neo4j.kernel.Uniqueness;

import java.io.IOException;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import static org.animotron.graph.AnimoGraph.getDb;
import static org.animotron.graph.Properties.RID;
import static org.animotron.graph.RelationshipTypes.RESULT;
import static org.neo4j.graphdb.Direction.OUTGOING;

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
		return new Calc();
    }
    
    class Calc extends OnQuestion {
	
		@Override
		public void act(final PFlow pf) throws IOException {

			//if (debug) System.out.println("GET "+Thread.currentThread());

			final Relationship op = pf.getOP();
			
			final Node node = op.getEndNode();
			
			final FastSet<Relationship> visitedREFs = FastSet.newInstance();
			final FastSet<Node> thes = FastSet.newInstance(); 
			try {
				Pipe p = AN.getREFs(pf, pf.getVector());
				QCAVector theNode = null;
				while ((theNode = p.take()) != null) {
					if (theNode.getClosest().isType(VALUE._)) {
						int num = VALUE.number(VALUE._.reference(theNode.getClosest())).intValue();
						QCAVector v = pf.getVector();
						while (num > 0) {
							v = v.getContext().get(0);
							num--;
						}
						System.out.println(v);
						relaxReference(pf, v, v.getQuestion());
//						pf.sendAnswer(v);
					} else {
						thes.add( theNode.getClosestEndNode() );
						System.out.println("THEs += "+theNode);
					}
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
				final Set<Relationship> visitedREFs) throws IOException {
			
			if (debug) { 
				Utils.debug(GET._, op, thes);
			}

			//check, maybe, result was already calculated
			if (!Utils.results(pf)) {
				//no pre-calculated result, calculate it
				
				OnContext onContext = new OnContext() {
					@Override
					public void onMessage(QCAVector vector) {
						super.onMessage(vector);
						
						if (vector == null)
							return;
						
						if (debug) { 
							System.out.println("GET on context "+Thread.currentThread());
							System.out.println("GET ["+op+"] vector "+vector);
						}
						
						get(pf, vector, thes, visitedREFs);
					}
				};
				//pf.answerChannel().subscribe(onContext);
				
				if (Utils.haveContext(pf)) {
					Evaluator.sendQuestion(pf.getController(), onContext, pf.getVector(), node);
					onContext.isDone();
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
						
						get(pf, refs, thes, visitedREFs, false);
					} finally {
						FastSet.recycle(refs);
					}
				}
			}
		};

	}
	
	public boolean get(PFlow pf, QCAVector vector, final Set<Node> thes, Set<Relationship> visitedREFs) {
		FastSet<QCAVector> refs = FastSet.newInstance();
		try {
			refs.add(vector);
			
			return get(pf, refs, thes, visitedREFs, true);
		} finally {
			FastSet.recycle(refs);
		}
	}

	private boolean check(
			final PFlow pf, 
			final QCAVector v, 
			final Relationship toCheck, 
			final Node middle, 
			final Set<Node> thes, 
			final Set<Relationship> visitedREFs,
			final boolean onContext) {
		
		if (toCheck == null) return false;
		
		if (visitedREFs.contains(toCheck)) return false;
		//if (toCheck.isType(REF._) && visitedREFs.contains(v.getQuestion())) return false;
		
		visitedREFs.add(toCheck);
		
		if (searchForHAVE(pf, Utils.relax(toCheck), v, middle, thes, onContext))
			return true;

		//if (!pf.isInStack(have[i])) {
		//set.add(new QCAVector(op, v, have[i]));

		return false;
	}

	public boolean get(
			final PFlow pf,
			final Set<QCAVector> REFs, 
			final Set<Node> thes, 
			Set<Relationship> visitedREFs,
			final boolean onContext) {
		
		if (visitedREFs == null) visitedREFs = new FastSet<Relationship>();
		
		FastSet<QCAVector> nextREFs = FastSet.newInstance();
		FastSet<QCAVector> newREFs = FastSet.newInstance();
		
		FastSet<QCAVector> tmp = null;

		try {
			nextREFs.addAll(REFs);
	
			boolean found = false;
			
//			Relationship t = null;
			
			while (true) {
				
				if (debug) {
					System.out.println("["+pf.getOP()+"] nextREFs ");
					Utils.debug(GET._, pf.getOP(), thes);
				}
	
				QCAVector v = null;
				for (FastSet.Record r = nextREFs.head(), end = nextREFs.tail(); (r = r.getNext()) != end;) {
					v = nextREFs.valueOf(r);
					
					if (debug) {
						System.out.println("checking "+v);
						Utils.debug(GET._, pf.getOP(), thes);
					}
					
					if (v.getQuestion() != null && v.hasAnswer()) {
						
						Node middle = null;
						Statement s = Statements.relationshipType(v.getQuestion());
						if (s instanceof ANY) {
							try {
								middle = v.getQuestion().getEndNode().getSingleRelationship(REF._, OUTGOING).getEndNode();
							} catch (Exception e) {
								e.printStackTrace();
							}
						}
						
						if (!check(pf, v, v.getUnrelaxedAnswer(), middle, thes, visitedREFs, onContext)) {
							//check other answers
							if (v.getAnswers() != null) {
								for (QCAVector vv : v.getAnswers()) {
									if (check(pf, vv, vv.getUnrelaxedAnswer(), middle, thes, visitedREFs, onContext))
										found = true;
								}
							}
						} else {
							found = true;
						}
					}
					
					if (found || !v.getQuestion().isType(AN._))
						visitedREFs.add(v.getQuestion());
				}
				
				if (found) return true;
	
				//check AN questions
				for (FastSet.Record r = nextREFs.head(), end = nextREFs.tail(); (r = r.getNext()) != end;) {
					v = nextREFs.valueOf(r);
					if (v.getQuestion().isType(AN._)) {
						if (check(pf, v, v.getQuestion(), null, thes, visitedREFs, onContext))
							found = true;
						visitedREFs.add(v.getQuestion());
					}
				}
				
				if (found) return true;

				for (FastSet.Record r = nextREFs.head(), end = nextREFs.tail(); (r = r.getNext()) != end;) {
					v = nextREFs.valueOf(r);

					List<QCAVector> cs = v.getContext();
					if (cs != null) {
						for (QCAVector c : cs) {
							checkVector(c, newREFs, visitedREFs);
						}
					}
					
//					t = v.getUnrelaxedAnswer();
//					if (t != null && !t.equals(v.getQuestion())) {
//						if (! t.isType(AN._))
//							getOutgoingReferences(pf, v, t, t.getStartNode(), newREFs, visitedREFs);
//						
//						getOutgoingReferences(pf, v, t, t.getEndNode(), newREFs, visitedREFs);
//					}
//
//					t = v.getQuestion();
//					if (t != null && t.isType(DEF._)) {
//						getOutgoingReferences(pf, v, t, t.getEndNode(), newREFs, visitedREFs);
//					}
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
	
//	private void getOutgoingReferences(PFlow pf, QCAVector vector, Relationship rr, Node node, Set<QCAVector> newREFs, Set<Relationship> visitedREFs) {
//
//		QCAVector cV = null;
//		IndexHits<Relationship> it = Order._.context(node);
//		try {
//			for (Relationship r : it) {
//				if (visitedREFs != null && visitedREFs.contains(r)) {
//					continue;
//				}
//	
//				cV = vector.question(r); 
//
//				Statement st = Statements.relationshipType(r);
//				if (st instanceof AN) {
//					Pipe p = AN.getREFs(pf, cV);
//					QCAVector v;
//					while ((v = p.take()) != null) {
//						Relationship t = v.getClosest();
//						
//						cV.addAnswer(v);
//						
//						if (visitedREFs != null && !visitedREFs.contains(t)) {
//							newREFs.add(v);
//						}
//					}
//	
//				} else if (st instanceof Reference) {
////					if (!pf.isInStack(r)) {
//						//System.out.println("["+pf.getOP()+"] evaluative "+prev);
//						Pipe in = Evaluator._.execute(pf.getController(), cV);
//						QCAVector v;
//						while ((v = in.take()) != null) {
//							cV.addAnswer(v);
//							if (visitedREFs != null && !visitedREFs.contains(v.getAnswer()))
//								newREFs.add(v);
//						}
////					}
//				}
//			}
//		} catch (Throwable t) {
//			pf.sendException(t);
//		} finally {
//			it.close();
//		}
//	}
	
	private boolean searchForHAVE(
			final PFlow pf, 
			final Relationship ref,
			final QCAVector v,
			final Node middle,
			final Set<Node> thes,
			final boolean onContext) {
		
		//search for inside 'HAVE'
		//return searchForHAVE(pf, ref, ref.getEndNode(), thes);
		if (getByHave(pf, v, ref, ref.getEndNode(), middle, thes, onContext))
			return true;

		//search for local 'HAVE'
		if (ref.isType(REF._)) {
			if (getByHave(pf, v, v.getQuestion(), ref.getStartNode(), middle, thes, onContext))
				return true;
		}

		return false;
	}
	
	private boolean eval(PFlow pf, QCAVector vector, Relationship op) throws Throwable {
        try {
            Pipe in = Evaluator._.execute(pf.getController(), vector.question(op));

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

	private boolean relaxReference(PFlow pf, QCAVector vector, Relationship op) {
        try{
            if (!(op.isType(ANY._) || op.isType(GET._))) {
                if (debug)
                    System.out.println("["+pf.getOP()+"] answered "+op);

//                pf.sendAnswer(pf.getVector().answered(op, vector), RESULT);
                //pf.sendAnswer(op);
                
                if (!Utils.haveContext(op.getEndNode())) {
                    return eval(pf, vector, op);
//                	System.out.println("SEND AS IT");
//                	pf.sendAnswer(pf.getVector().answered(op, vector), RESULT);
                } else {
		    		IndexHits<Relationship> hits = Order._.context(op.getEndNode());
		    		try {
		    			for (Relationship rr : hits) {
//		                    return eval(pf, vector, rr);
		    				pf.sendAnswer(pf.getVector().answered(rr, vector), RESULT);
		    			}
		    		} finally {
		    			hits.close();
		    		}
                }
                
                return true;
            }

            if (debug)
                System.out.println("["+pf.getOP()+"] Relaxing "+op+" @ "+vector);

            return eval(pf, vector, op);
        } catch (Throwable t) {
            pf.sendException(t);
        }
        return false;
	}
	
	private boolean haveMiddle(Path path, Node middle) {
		for (Node n : path.nodes()) {
			if (n.equals(middle))
				return true;
		}
		return false;
	}

	private static TraversalDescription prepared = 
			Traversal.description().
			depthFirst().
			uniqueness(Uniqueness.RELATIONSHIP_PATH);
//			relationships(ASHIFT._, OUTGOING).
//			
//			relationships(AN._, BOTH).
//			relationships(REF._, OUTGOING).
//
//			relationships(ANY._, OUTGOING).
//			relationships(ALL._, OUTGOING).
//			relationships(PREFER._, OUTGOING).
//			relationships(GET._, OUTGOING);
//			.
//			relationships(SHALL._, OUTGOING);
	
	static FastSet<RelationshipType> allowedSet = new FastSet<RelationshipType>();
	static {
		allowedSet.add(ASHIFT._);
		allowedSet.add(REF._);

		allowedSet.add(AN._);
		
		allowedSet.add(ANY._);
		allowedSet.add(ALL._);
		allowedSet.add(PREFER._);
		
		allowedSet.add(GET._);
	}
	
	private boolean getByHave(
			final PFlow pf, 
			final QCAVector vector, 
			final Relationship op, 
			final Node context, 
			final Node middle, 
			final Set<Node> thes, 
			final boolean onContext) {
		
		if (context == null) return false;
		
		if (debug)
			System.out.println("middle "+middle);
		
		TraversalDescription trav = prepared.
		expand(new PathExpander<Long>() {

			@Override
			public Iterable<Relationship> expand(Path path, BranchState<Long> state) {
//				System.out.println(path);
				
//				if (path.length() == 0) {
//					return Order._.queryDownIterable(path.endNode(), allowedSet);
//				}

				final Node node;
				
				Relationship r = path.lastRelationship();
				if (r != null && r.isType(ASHIFT._)) {
					node = getDb().getRelationshipById((Long) RID.get(r)).getEndNode();

				} else {
					node = path.endNode();
				}
					
				long defId = state.getState();
				if (vector.lastDefId() != -1) {
					defId = vector.lastDefId();
				}	
				if (Properties.DEFID.has(node)) {
					defId = (Long) Properties.DEFID.get(node);
				}
				state.setState(defId);
				
				if (defId > -1) {
					final Relationship ashift = AShift._.get(node, defId);
					if (ashift != null) {
						return new Iterable<Relationship>() {
							@Override
							public Iterator<Relationship> iterator() {
								return new IteratorSingle(ashift);
							}
						};
					}
				}

				return Order._.queryDownIterable(node, allowedSet);
			}

			@Override
			public PathExpander<Long> reverse() {
				return null;
			}
			
		}, 
		new InitialStateFactory<Long>() {

			@Override
			public Long initialState(Path path) {
//				System.out.println("state "+path);
				return (long) -1;
			}
			
		}).
		evaluator(new Searcher(){
			@Override
			public Evaluation evaluate(Path path) {
				if (debug)
					System.out.println(path);
				
				if (path.length() == 0)
					return Evaluation.EXCLUDE_AND_CONTINUE;
				
				Relationship r = path.relationships().iterator().next();
				if (r.equals(op))
					return Evaluation.EXCLUDE_AND_PRUNE;
				
				return _evaluate_(path, thes);
			}
		});
		
		FastMap<Relationship, List<Path>> paths = FastMap.newInstance();
		try {
	
			boolean middlePresent = false;
			List<Path> l;
			for (Path path : trav.traverse(context)) {
				if (debug) 
					System.out.println("["+pf.getOP()+"] * "+path);
				
				if (path.length() == 1) {
					if (op == null) {
						System.out.println("WARNING: DONT KNOW OP");
						continue;
					}
					
					if (pf.getOP().equals(op))
						continue;
					
					l = new FastList<Path>();
					l.add(path);
					paths.put(op, l);
					
					continue;
				}
				
				if (path.length() == 2) {
					//UNDERSTAND: should we check context
					Relationship r = path.relationships().iterator().next();
					if (pf.getVector().getQuestion().getId() == r.getId())
						continue;
					
					if (relaxReference(pf, vector, r)) {
						if (debug)
							System.out.println("+++TRUE+++");
						return true;
					}
				}
	
				Relationship fR = path.relationships().iterator().next();
				List<Path> ps = paths.get(fR);
				if (ps == null) {// || p.length() > path.length()) {
					
					boolean thisMiddle = haveMiddle(path, middle);
					if (middlePresent) {
						if (thisMiddle) {
							l = new FastList<Path>();
							l.add(path);
		
							paths.put(fR, l);
						}
					} else {
						if (thisMiddle) {
							middlePresent = thisMiddle;
							paths.clear();
						}
						
						l = new FastList<Path>();
						l.add(path);
	
						paths.put(fR, l);
					}
				} else {
					l = paths.get(fR);
					
					if (l.get(0).length() > path.length()) {
						
						middlePresent = haveMiddle(path, middle);
						l.clear();
						l.add(path);
						
					} else if (l.get(0).length() == path.length()) {
						boolean thisMiddle = haveMiddle(path, middle);
						if (middlePresent) {
							if (thisMiddle)
								l.add(path);
						} else {
							if (thisMiddle) {
								middlePresent = thisMiddle;
								l.clear();
								l.add(path);
							
							} else {
								l.add(path);
							}
						}
					}
				}
			}
			
			if (paths.isEmpty()) {
				if (debug)
					System.out.println("+++FALSE+++");
				return false;
			}
			
			Relationship startBy = null;
			
			int refs = 0;
			int ANs = 0;
			//if (op.isType(RESULT)) ANs++;
			
			Relationship res = null;
			Relationship prevRes = null;
			Relationship prevPrevRes = null;
			FastTable<Relationship> resByHAVE = FastTable.newInstance();
			FastTable<Relationship> resByIS = FastTable.newInstance();
			try {
				for (List<Path> ps : paths.values()) {
					for (Path path : ps) {
						res = null; prevRes = null; prevPrevRes = null;
	
						if (path.length() == 1 && path.lastRelationship().isType(REF._)) {
							res = op;
	
						} else {
							Iterator<Relationship> it = path.relationships().iterator();
							for (Relationship r = null; it.hasNext(); ) {
								r = it.next();
								if (r.isType(ASHIFT._))
									continue;
								
								if (startBy == null)
									startBy = r;
									
								if (!pf.isInStack(r)) {
									if (r.isType(AN._)) {
										res = r;
										ANs++;
									} else {
										if (ANs > 1) {
											//check is it pseudo HAVE or IS topology. on HAVE return it else last of top 
											if (r.isType(REF._) && it.hasNext()) {
												r = it.next();
												if (r.isType(AN._) && Utils.haveContext(r.getEndNode()))
													res = r;
											}
											break;
										}
										ANs = 0;
											
										if (r.isType(ANY._) || r.isType(ALL._) || r.isType(PREFER._)) {
//											if (it.hasNext() && it.next().isType(REF._) && !it.hasNext()) {
												res = r;
//											} else {
//												res = null;
//											}
											break;
					
										} else if (r.isType(GET._)) {
											if (it.hasNext())
												if (it.next().isType(REF._) && !it.hasNext())
													res = r;
											break;
			
										} else if (r.isType(SHALL._)) {
											res = r;
			
										} else if (r.isType(REF._)) {
											//ignore pseudo IS
											if (Utils.haveContext(r.getStartNode())) {
												prevRes = null;
												
												if (onContext) break;
												else if (refs > 1) break;
												
												refs++;
											} else {
												prevPrevRes = prevRes;
												prevRes = res;
											}
										}
									}
									
								}
							}
						}
						if (prevPrevRes != null) res = prevPrevRes;
						
						if (res != null) {
							if (startBy != null && startBy.isType(REF._))
								resByIS.add(res);
							else
								resByHAVE.add(res);
						}
						startBy = null;
					}
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
			System.out.println("***TRUE***");
			return true;
		} finally {
			FastMap.recycle(paths);
		}
	}
}
