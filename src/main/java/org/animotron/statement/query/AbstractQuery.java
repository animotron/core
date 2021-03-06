/*
 *  Copyright (C) 2011-2013 The Animo Project
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

import javolution.util.FastSet;
import javolution.util.FastTable;
import org.animotron.graph.index.Order;
import org.animotron.manipulator.PFlow;
import org.animotron.manipulator.QCAVector;
import org.animotron.statement.Statement;
import org.animotron.statement.Statements;
import org.animotron.statement.operator.*;
import org.animotron.statement.relation.USE;
import org.animotron.statement.relation.WEAK_USE;
import org.neo4j.graphdb.*;
import org.neo4j.graphdb.index.IndexHits;
import org.neo4j.graphdb.traversal.Evaluation;
import org.neo4j.graphdb.traversal.Evaluator;
import org.neo4j.graphdb.traversal.TraversalDescription;
import org.neo4j.kernel.Traversal;
import org.neo4j.kernel.Uniqueness;

import java.util.Iterator;
import java.util.List;
import java.util.Set;

import static org.animotron.graph.Properties.FREEZE;
import static org.animotron.graph.RelationshipTypes.RESULT;
import static org.neo4j.graphdb.Direction.INCOMING;
import static org.neo4j.graphdb.Direction.OUTGOING;
import static org.neo4j.graphdb.traversal.Evaluation.*;

/**
 * Abstract Query
 * 
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 */
public abstract class AbstractQuery extends Operator implements Evaluable, Query {
	
	private static boolean debugUSE = false;

    public AbstractQuery(String... name) {
        super(name);
    }

    protected static TraversalDescription td_IS =
        Traversal.description().
            breadthFirst().
            relationships(REF._, INCOMING ).
            relationships(AN._, INCOMING ).
            //evaluator(Evaluators.excludeStartPosition()).
            evaluator(new org.neo4j.graphdb.traversal.Evaluator(){
    			@Override
    			public Evaluation evaluate(Path path) {
    				//System.out.println(fs);
    				
    				if (path.length() < 2)
    					return EXCLUDE_AND_CONTINUE;
    				
    				Relationship r = path.lastRelationship();
    				if (r.isType(REF._) && FREEZE.has(r))
    					return EXCLUDE_AND_PRUNE;

    				if (path.length() % 2 == 0 && r.isType(AN._))
    					return INCLUDE_AND_CONTINUE;
    				
    				if (path.length() % 2 == 1 && !r.isType(REF._))
    					return EXCLUDE_AND_PRUNE;
    					

    				return EXCLUDE_AND_CONTINUE;
    			}
    		});

    protected static TraversalDescription td_IS_leaf =
            Traversal.description().
                breadthFirst().
                relationships(REF._, INCOMING ).
                relationships(AN._, INCOMING ).
//                relationships(ASHIFT._, INCOMING ).
                //evaluator(Evaluators.excludeStartPosition()).
                evaluator(new org.neo4j.graphdb.traversal.Evaluator(){
        			@Override
        			public Evaluation evaluate(Path path) {
        				if (path.endNode().getId() == 0)
        					return EXCLUDE_AND_PRUNE;
        				
//        				System.out.println(fs);
        				
        				if (path.length() < 1)
        					return EXCLUDE_AND_CONTINUE;
        				
        				Relationship r = path.lastRelationship();
        				switch (path.length() % 2) {
//						case 0:
//	        				if (!r.isType(ASHIFT._))
//	        					return EXCLUDE_AND_PRUNE;
//							break;

						case 1:
	        				if (!r.isType(REF._) || FREEZE.has(r))
	        					return EXCLUDE_AND_PRUNE;
							break;

						case 0:
	        				if (!r.isType(AN._))
	        					return EXCLUDE_AND_PRUNE;
	        				
        					if (!NONSTOP.is(r))
        						return INCLUDE_AND_PRUNE;

        					return INCLUDE_AND_CONTINUE;

						default:
							break;
						}

        				return EXCLUDE_AND_CONTINUE;
        			}
        		});
    
    protected static TraversalDescription td_IS_down =
            Traversal.description().
                breadthFirst().
                relationships(REF._, OUTGOING ).
                relationships(AN._, OUTGOING ).
                //evaluator(Evaluators.excludeStartPosition()).
                evaluator(new org.neo4j.graphdb.traversal.Evaluator(){
        			@Override
        			public Evaluation evaluate(Path path) {
        				//System.out.println(" "+fs);
        				
        				if (path.length() < 1)
        					return EXCLUDE_AND_CONTINUE;
        				
        				Relationship r = path.lastRelationship();
        				if (r.isType(REF._) && FREEZE.has(r))
        					return EXCLUDE_AND_PRUNE;
        				
        				if (path.length() % 2 == 0 && !r.isType(AN._))
        					return EXCLUDE_AND_PRUNE;
        				
        				if (path.length() % 2 == 1 && r.isType(REF._))
        					return INCLUDE_AND_CONTINUE;
        					

        				return EXCLUDE_AND_CONTINUE;
        			}
        		});
    
    protected static TraversalDescription THE_by_REF =
            Traversal.description().
                breadthFirst().
                relationships(REF._, INCOMING ).
                relationships(AN._, INCOMING ).
                //evaluator(Evaluators.excludeStartPosition()).
                evaluator(new org.neo4j.graphdb.traversal.Evaluator(){
        			@Override
        			public Evaluation evaluate(Path path) {
        				if (path.length() == 0)
        					return EXCLUDE_AND_CONTINUE;
        					
        				Relationship r = path.lastRelationship();
        				if (r.isType(REF._) && FREEZE.has(r))
        					return EXCLUDE_AND_PRUNE;
        				
        				if (path.length() == 1 && r.isType(REF._))
    						return EXCLUDE_AND_CONTINUE;
        					
        				if (path.length() == 2 && r.isType(DEF._))
    						return INCLUDE_AND_PRUNE;
        				
    					return EXCLUDE_AND_PRUNE;
        			}
        		});


    protected boolean filtering(final PFlow pf, final Relationship ref, final Set<Node> uses, final Set<Node> weaks) {
    	if (ref == null) {
//    		System.out.println("AbstractQuery: NULL!!!");
    		return false;
    	}
//    	System.out.println("filtering: "+ref+" "+Arrays.toString(uses.toArray()));
    	return filtering(pf, ref, ref.getEndNode(), uses, weaks);
    }

    protected boolean filtering(final PFlow pf, final Relationship ref, final Node toCheckByUSE, final Set<Node> uses, final Set<Node> weaks) {
    	if (!setFiltering(toCheckByUSE, uses, weaks))
    		return false;

    	IndexHits<Relationship> hits = Order._.context(pf.getOPNode());
    	try {
	        for (Relationship r : hits) {
	            Statement st = Statements.relationshipType(r);
	            if (st instanceof Predicate) {
	                try {
	                    if (!((Predicate) st).filter(pf, r, ref))
	                        return false;
	                } catch (Throwable t) {
	                    //XXX: report
	                    t.printStackTrace();
	                    return false;
	                }
	            }
	        }
    	} finally {
    		hits.close();
    	}
        return true;
    }

    protected boolean setFiltering(final Node toCheckByUSE, final Set<Node> uses, final Set<Node> weaks) {
    	if (!uses.isEmpty() || !weaks.isEmpty()) {
    		//check intersection
    		FastSet<Node> uSet = FastSet.newInstance();
    		FastSet<Node> wSet = FastSet.newInstance();
    		try {
    			uSet.addAll(uses);
    			wSet.addAll(weaks);

	    		TraversalDescription td = getIntersectionChecktravers(uSet, wSet, weaks.size(), false);
	    		if (!td.traverse(toCheckByUSE).iterator().hasNext()) {
	    			if (debugUSE) 
	    				System.out.println("filtered out by USE "+uses);
	    			return false;
	    		}
    		} finally {
    			FastSet.recycle(uSet);
    			FastSet.recycle(wSet);
    		}
    	}
    	return true;
    }
    
    private void searchForUSE(final Set<Node> uses, final Set<Node> weaks, final List<QCAVector> vectors, final Set<QCAVector> visitred) {
    	if (vectors == null) return;
    	
		for (QCAVector vector : vectors) {
			searchForUSE(uses, weaks, vector, visitred);
		}
    }

    private void collectUSEs(final RelationshipType type, final Set<Node> set, final Node node) {
		for (Relationship r : node.getRelationships(OUTGOING, type)) {
			for (Relationship rr : r.getEndNode().getRelationships(OUTGOING, REF._))
				if (!FREEZE.has(rr))
					set.add(rr.getEndNode());
		}
    }

    private void collectUSEs(final Set<Node> uses, final Set<Node> weaks, Node node) {
    	collectUSEs(USE._, uses, node);
    	collectUSEs(WEAK_USE._, weaks, node);
    }

    private void checkVectorForUSE(final Set<Node> uses, final Set<Node> weaks, final QCAVector vector, final Set<QCAVector> visitred) {
    	if (vector == null)
    		return;

		if (vector.getQuestion() != null)
			collectUSEs(uses, weaks, vector.getQuestion().getEndNode());

		if (vector.getUnrelaxedAnswer() != null)
			collectUSEs(uses, weaks, vector.getAnswerEndNode());

		searchForUSE(uses, weaks, vector.getAnswers(), visitred);
    }
    
    private void searchForUSE(final Set<Node> uses, final Set<Node> weaks, final QCAVector vector, final Set<QCAVector> visitred) {
    	//System.out.println("searchForUSE "+vector);
    	QCAVector prev = vector;
    	while (prev != null) {
    		
    		if (visitred.contains(prev))
    			break;
    		visitred.add(prev);
    		
	    	checkVectorForUSE(uses, weaks, prev, visitred);
			searchForUSE(uses, weaks, prev.getContext(), visitred);
			
	    	prev = null;//prev.getPrecedingSibling();
    	}
    	
    }
    
	protected void getUSEs(PFlow pf, Node theNode, final Set<Node> uses, final Set<Node> weaks, final Set<Path> directed) {

    	final FastSet<QCAVector> visited = FastSet.newInstance();
    	final FastSet<Node> allUses = FastSet.newInstance();
    	final FastSet<Node> allWeaks = FastSet.newInstance();
    	final FastSet<Node> weakest = FastSet.newInstance();
    	try {
			searchForUSE(allUses, allWeaks, pf.getVector(), visited);
			
//			System.out.println(pf.getVector());
//			System.out.println("allUses : "+Arrays.toString(allUses.toArray()));
//			System.out.println("allWeaks: "+Arrays.toString(allWeaks.toArray()));
			
	    	if (allUses.isEmpty() && allWeaks.isEmpty()) return;
	    		
	    	TraversalDescription trav = td.breadthFirst().
					relationships(AN._, INCOMING).
					relationships(REF._, INCOMING).
					relationships(DEF._, INCOMING).
					relationships(ASHIFT._, INCOMING).
			evaluator(new IntersectionSearcher(){
				@Override
				public Evaluation evaluate(Path path) {
//					System.out.println(" "+fs);
					return _evaluate_(path, allUses, allWeaks, uses, weaks, weakest, directed);
				}
			});
	    	
	    	for (Path path : trav.traverse(theNode)) {
	    		//System.out.println(" * use * "+fs);
	    	}
	    	
	    	if (uses.isEmpty())
	    		weaks.addAll(weakest);
	
	    	if (allUses.contains(theNode))
				uses.add(theNode);

	    	if (allWeaks.contains(theNode))
	    		weaks.add(theNode);
	    	
    	} finally {
    		FastSet.recycle(allUses);
    		FastSet.recycle(allWeaks);
    		FastSet.recycle(visited);
    		FastSet.recycle(weakest);
    	}
    }
	
	protected Node getClosestIntersection(FastSet<Path> paths) {
		FastTable<Relationship> commonPath = FastTable.newInstance();
		
		try {
			for (FastSet.Record r = paths.head(), end = paths.tail(); (r = r.getNext()) != end;) {
				Path path = paths.valueOf(r);
				
				if (!commonPath.isEmpty()) {
					Iterator<Relationship> curIt = path.relationships().iterator();
					for (int i = 0, size = commonPath.size(); i < size; i++) {
						if (curIt.hasNext()) {
							if (commonPath.get(i).equals(curIt.next()))
								continue;
							
							commonPath.removeRange(i, commonPath.size());
							break;
						}
						
					}
				} else {
					for (Relationship rr : path.relationships())
						commonPath.add(rr);
				}
			}
			if (!commonPath.isEmpty()) {
				if (commonPath.getLast().isType(AN._))
					return commonPath.getLast().getStartNode();
				else
					return commonPath.getLast().getEndNode();
			}
		} finally {
			FastTable.recycle(commonPath);
		}
		return null;
	}

	protected TraversalDescription getIntersectionChecktravers(final Set<Node> mustHave, final Set<Node> shouldHave, final int was, final boolean underAREV) {

		return Traversal.description().depthFirst().
		uniqueness(Uniqueness.RELATIONSHIP_PATH).
		evaluator(new org.neo4j.graphdb.traversal.Evaluator(){
			@Override
			public Evaluation evaluate(Path path) {
				System.out.println(" "+path);

				Node sNode;
				if (path.length() == 0) {
					sNode = path.startNode();
				} else {
					
					Relationship lastR = path.lastRelationship();
					
					if (lastR.isType(REF._) && FREEZE.has(lastR))
						return EXCLUDE_AND_PRUNE;
					
					Relationship firstR = firstRelationsip(path);
					if (firstR.isType(AN._)) {
						switch (path.length() % 2) {
//						case 0:
//		    				if (!lastR.isType(ASHIFT._))
//		    					return EXCLUDE_AND_PRUNE;
//							
//							break;
//
						case 1:
		    				if (!lastR.isType(AN._))
		    					return EXCLUDE_AND_PRUNE;
							
							break;

						case 0:
		    				if (!lastR.isType(REF._))
		    					return EXCLUDE_AND_PRUNE;
							
							break;

						default:
							break;
						}
					
					} else if (firstR.isType(REF._)) {
						switch (path.length() % 2) {
						case 0:
		    				if (!lastR.isType(AN._))
		    					return EXCLUDE_AND_PRUNE;
							
							break;

						case 1:
		    				if (!lastR.isType(REF._))
		    					return EXCLUDE_AND_PRUNE;
							
							break;

//						case 2:
//		    				if (!lastR.isType(ASHIFT._))
//		    					return EXCLUDE_AND_PRUNE;
//							
//							break;

						default:
							break;
						}
	    				
//					} else if (firstR.isType(ASHIFT._)) {
//						switch (fs.length() % 3) {
//						case 0:
//		    				if (!lastR.isType(REF._))
//		    					return EXCLUDE_AND_PRUNE;
//							
//							break;
//
//						case 1:
//		    				if (!lastR.isType(ASHIFT._))
//		    					return EXCLUDE_AND_PRUNE;
//							
//							break;
//
//						case 2:
//		    				if (!lastR.isType(AN._))
//		    					return EXCLUDE_AND_PRUNE;
//							
//							break;
//
//						default:
//							break;
//						}
//
					} else {
						return EXCLUDE_AND_PRUNE;
					}

					sNode = lastR.getEndNode();
					if (!sNode.equals(path.endNode()))
						return EXCLUDE_AND_PRUNE;
				}
				
				if (shouldHave != null) shouldHave.remove(sNode);
				//System.out.println(Arrays.toString(mustHave.toArray()));
				mustHave.remove(sNode);
				if (mustHave.size() == 0) {
					if (was == 0 || (shouldHave == null || shouldHave.size() != was))
						return INCLUDE_AND_PRUNE;
				}

				return EXCLUDE_AND_CONTINUE;
			}

			private Relationship firstRelationsip(Path path) {
				return path.relationships().iterator().next();
			}
		});
	};

	protected TraversalDescription getUSEtravers(final Relationship end) {

		return Traversal.description().depthFirst().
		//uniqueness(Uniqueness.RELATIONSHIP_PATH).
		uniqueness(Uniqueness.NODE_PATH).
		evaluator(new org.neo4j.graphdb.traversal.Evaluator(){
			@Override
			public Evaluation evaluate(Path path) {
				//System.out.println(" "+fs);
				if (path.length() > 0) {
					Relationship r = path.lastRelationship();
					if (r.getEndNode().getId() == 1 || r.isType(RESULT))
						return EXCLUDE_AND_PRUNE;
						
					if (r.getStartNode().equals(path.endNode())) {
						if (r.equals(end)) {
							return INCLUDE_AND_PRUNE;
						} 

						if (path.length() == 1 && !(r.isType(USE._) || r.isType(AN._))) {
							return EXCLUDE_AND_PRUNE;
						}
						return EXCLUDE_AND_CONTINUE;
					
					//Allow IS<-USE<-IS->... and IS<-IS->...<-USE 
					} if (path.length() > 1 && r.isType(AN._)) {
						return EXCLUDE_AND_CONTINUE;
					}
					return EXCLUDE_AND_PRUNE;
				}
				return EXCLUDE_AND_CONTINUE;
			}
		});
	};

	protected TraversalDescription getDirectedTravers(final Node end) {

		return Traversal.description().depthFirst().
		uniqueness(Uniqueness.RELATIONSHIP_PATH).
		evaluator(new org.neo4j.graphdb.traversal.Evaluator(){
			@Override
			public Evaluation evaluate(Path path) {
				if (path.length() > 0) {
					Relationship r = path.lastRelationship(); 
					if (r.getStartNode().equals(path.endNode())) {
						if (r.getStartNode().equals(end)) {
							return INCLUDE_AND_PRUNE;
						} 
						return EXCLUDE_AND_CONTINUE;	
					} 
					return EXCLUDE_AND_PRUNE;
				}
				return EXCLUDE_AND_CONTINUE;
			}
		});
	};
	
	TraversalDescription td = Traversal.description().
			depthFirst().uniqueness(Uniqueness.RELATIONSHIP_PATH);

	protected abstract class Searcher implements org.neo4j.graphdb.traversal.Evaluator {

		public Evaluation _evaluate_(Path path, Set<Node> targets) {
//			System.out.println(fs);
			
			if (path.length() == 0)
				return EXCLUDE_AND_CONTINUE;
			
			Relationship r = path.lastRelationship();
			if (r.isType(ASHIFT._))
				return EXCLUDE_AND_CONTINUE;

			if (r.isType(REF._) && FREEZE.has(r))
				return EXCLUDE_AND_PRUNE;
			
			if (path.length() == 1) {
				if (r.isType(REF._) && targets.contains(r.getEndNode()))
					return INCLUDE_AND_PRUNE;

				return EXCLUDE_AND_CONTINUE;
				
			} else if (path.length() >= 2) {
				if (r.isType(REF._)) {
					Node node = r.getEndNode();
					if (targets.contains(node)) {
						
						//XXX: hack, fix required!
						if (AN.beginWithHasA(path))
							return INCLUDE_AND_PRUNE;

						//check for NONSTOP sign
//						Relationship b = null; int countSTOPPER = 0;
//						for (Relationship s : fs.relationships()) {
//							if (countSTOPPER > 0)
//								return EXCLUDE_AND_PRUNE;
//								
//							if (s.isType(REF._)) {
//								if (b != null && !NONSTOP.is(b))
//									countSTOPPER++;
//							}
//							b = s;
//						}

						return INCLUDE_AND_PRUNE;
					}

					if (r.equals(AN.endOfHasA(path)))
						return EXCLUDE_AND_CONTINUE;

					//check for NONSTOP sign
//					Relationship b = null;
//					for (Relationship s : fs.relationships()) {
//						if (s.equals(r)) {
//							if (b != null && !NONSTOP.is(b))
//								return EXCLUDE_AND_PRUNE;
//						}
//						b = s;
//					}
					
					return EXCLUDE_AND_CONTINUE;
				
				} else if (r.isType(AN._)) {
					Iterator<Relationship> it = path.reverseRelationships().iterator();
					if (it.hasNext()) {
						it.next();
						if (it.hasNext()) {
							final Relationship before = it.next();
							if (before.isType(AN._) && !NONSTOP.is(before)) {
								return EXCLUDE_AND_PRUNE;
							}
						}
					} else {
						return EXCLUDE_AND_PRUNE;
					}

					Node endNode = r.getEndNode();
					if (endNode.equals(path.endNode())) {
						if (AN.beginWithHasA(path)) {
							boolean haveAREV = false;
							for (Relationship rr : path.relationships())
								if (rr.isType(ASHIFT._))
									haveAREV = true;
							if (!haveAREV)
								return EXCLUDE_AND_PRUNE;
						}
						//must be empty to be IS-A
//						if (!Utils.haveContext(endNode))
							return EXCLUDE_AND_CONTINUE;
					} else {
						//check top HAS-A structure
						if (AN.endOfHasA(path) == null)
							return EXCLUDE_AND_CONTINUE;
					}
					return EXCLUDE_AND_PRUNE;
					
				} else if (r.isType(ANY._)  || r.isType(ALL._)  || r.isType(PREFER._)) {
					if (!r.getEndNode().equals(path.endNode()))
						return EXCLUDE_AND_PRUNE;
					
					for (Relationship rr : r.getEndNode().getRelationships(REF._, OUTGOING)) {
						Node node = rr.getEndNode();
						
						if (targets.contains(node))
							return INCLUDE_AND_PRUNE;
						
						for (Path p : td_IS_leaf.traverse(node)) {
//							System.out.println(p);
							if (targets.contains( p.lastRelationship().getStartNode()))
								return INCLUDE_AND_PRUNE;
						}
					}
				}
			}
			return EXCLUDE_AND_PRUNE;
		}
	}

	abstract class IntersectionSearcher implements Evaluator {
		
		private boolean haveUSE(final Path path, final Set<Node> intersection) {
			if (intersection.isEmpty()) return false;
			
			for (Node node : path.nodes()) {
				if (intersection.contains(node)) {
					return true;
				}
			}
			return false;
		}

		private void checkTHEnode(final Node n, final Path path, final Set<Node> targets, final Set<Node> weakTargets, final Set<Node> intersection, final Set<Node> weakIntersection, final Set<Node> weakestIntersection, final Set<Path> directed) {
			if (targets.contains(n)) {
				if (debugUSE) 
					System.out.println(" =>"+path);
					
				intersection.add(n);

				directed.add(path);//n
			
			} else if (weakTargets.contains(n)) {
				if (debugUSE) 
					System.out.println("[?weak] =>"+path);

				weakestIntersection.add(n);
				if (haveUSE(path, intersection)) {

					if (debugUSE) 
						System.out.println("[weak] =>"+path);
					
					weakIntersection.add(n);

					directed.add(path);//n
				}
			}
		}

		public Evaluation _evaluate_(Path path, final Set<Node> targets, final Set<Node> weakTargets, final Set<Node> intersection, final Set<Node> weakIntersection, final Set<Node> weakestIntersection, final Set<Path> directed) {
			if (path.length() < 1)
				return EXCLUDE_AND_CONTINUE;
			
			final Relationship r = path.lastRelationship();
			
			if (r.isType(REF._) && FREEZE.has(r))
				return EXCLUDE_AND_PRUNE;

			if (r.isType(DEF._)) {
				checkTHEnode(r.getEndNode(), path, targets, weakTargets, intersection, weakIntersection, weakestIntersection, directed);

//			} else if (fs.length() % 3 == 0) {
//				if (!r.isType(ASHIFT._))
//					return EXCLUDE_AND_PRUNE;
//				
//				return EXCLUDE_AND_CONTINUE;
//
			} else if (path.length() % 2 == 0) {
				if (!r.isType(AN._))
					return EXCLUDE_AND_PRUNE;
				
				Node node = r.getStartNode();
				try {
					checkTHEnode(DEF._.getDef(node), path, targets, weakTargets, intersection, weakIntersection, weakestIntersection, directed);
				} catch (Exception e) {}
				
				final FastSet<Node> use = FastSet.newInstance();
				final FastSet<Node> weakest = FastSet.newInstance();
				try {
			    	TraversalDescription trav = td.
							relationships(AN._, OUTGOING).
							relationships(REF._, OUTGOING).
							relationships(DEF._, OUTGOING).
					evaluator(new DownIntersectionSearcher(){
						@Override
						public Evaluation evaluate(Path path) {
							//XXX: make this work
	//						if (fs.length() == 1 && fs.lastRelationship().equals(r)) {
	//							System.out.println(" - "+fs+" - ");
	//							return EXCLUDE_AND_PRUNE;
	//						}
							
							//System.out.println(" - "+fs);
							return _evaluate_(path, haveUSE(path, intersection), targets, weakTargets, use, weakIntersection, weakest);
						}
					});
	
					if (debugUSE) 
						System.out.println(" - "+path);
					
			    	for (Path p : trav.traverse(node)) {
	
			    		//System.out.println(fs);
			    		//System.out.println(" ** "+p);
	
						directed.add(path);//r.getStartNode()
			    	}
			    	
			    	if (!use.isEmpty()) {
			    		intersection.addAll(use);
			    		weakIntersection.addAll(weakest);
			    	}
			    	
			    	weakestIntersection.addAll(weakest);
				} finally {
					FastSet.recycle(use);
					FastSet.recycle(weakest);
				}
		    	
		    	if (Utils.haveContext(r.getEndNode()))
		    		return EXCLUDE_AND_PRUNE;

			} else if (path.length() % 2 == 1)
				if (!r.isType(REF._))
					return EXCLUDE_AND_PRUNE;
				else {
					checkTHEnode(r.getEndNode(), path, targets, weakTargets, intersection, weakIntersection, weakestIntersection, directed);
				}
				

			return EXCLUDE_AND_CONTINUE;
		}
	};

	abstract class DownIntersectionSearcher implements Evaluator {
		
		private Evaluation checkTHEnode(final Node n, final Path path, final boolean allowWeak, final Set<Node> targets, final Set<Node> weakTargets, final Set<Node> intersection, final Set<Node> weakIntersection, final Set<Node> weakestIntersection) {
			if (targets.contains(n)) {
				if (debugUSE) 
					System.out.println(" ~> "+path);
				intersection.add(n);
				return INCLUDE_AND_PRUNE;
			
			} else if (weakTargets.contains(n)) {
				if (debugUSE) 
					System.out.println("[?weak] ~> "+path);
				
				if (allowWeak) {
					if (debugUSE) 
						System.out.println("[weak] ~> "+path);
					weakIntersection.add(n);
					return INCLUDE_AND_PRUNE;
				}
				weakestIntersection.add(n); 
			}
			return EXCLUDE_AND_PRUNE;
		}

		public Evaluation _evaluate_(final Path path, final boolean allowWeak, final Set<Node> targets, final Set<Node> weakTargets, final Set<Node> intersection, final Set<Node> weakIntersection, final Set<Node> weakestIntersection) {
			if (path.length() < 2)
				return EXCLUDE_AND_CONTINUE;
			
			Relationship r = path.lastRelationship();
			if (r.isType(REF._) && FREEZE.has(r))
				return EXCLUDE_AND_PRUNE;
				
			
			if (r.isType(DEF._)) {
				return checkTHEnode(r.getEndNode(), path, allowWeak, targets, weakTargets, intersection, weakIntersection, weakestIntersection);
			
			} else if (path.length() % 2 == 1) {
				if (!r.isType(AN._))
					return EXCLUDE_AND_PRUNE;
				
			} else if (path.length() % 2 == 0)
				if (!r.isType(REF._))
					return EXCLUDE_AND_PRUNE;
				else {
					return checkTHEnode(r.getEndNode(), path, allowWeak, targets, weakTargets, intersection, weakIntersection, weakestIntersection);
				}
				

			return EXCLUDE_AND_CONTINUE;
		}
	};

	abstract class ISearcher implements Evaluator {

		public Evaluation _evaluate_(final Path path, final Set<Node> targets) {
			//System.out.println(fs);
			
			if (path.length() < 2)
				return EXCLUDE_AND_CONTINUE;
			
			Relationship r = path.lastRelationship();
			if (r.isType(REF._) && FREEZE.has(r))
				return EXCLUDE_AND_PRUNE;

			if (path.length() == 1) {
				if (!r.isType(REF._))
					return EXCLUDE_AND_PRUNE;
				else if (path.endNode().equals(r.getStartNode())) {
					return EXCLUDE_AND_CONTINUE;
				}
				return EXCLUDE_AND_PRUNE;
			}

			if (r.isType(AN._)) {
				if (path.endNode().equals(r.getEndNode()))
					return EXCLUDE_AND_CONTINUE;
				else
					return EXCLUDE_AND_PRUNE;
			}
			
			Node n = r.getEndNode();
			if (r.isType(REF._) && targets.contains(n) && path.endNode().equals(n)){
				return INCLUDE_AND_PRUNE;
			}

			return EXCLUDE_AND_PRUNE;
		}
	};

	
    protected boolean isLeaf(Node node) {
		for (Relationship r : node.getRelationships(Direction.INCOMING, REF._))
			if (!FREEZE.has(r))
				for (Relationship rr : r.getStartNode().getRelationships(Direction.INCOMING, AN._))
					if (rr.getStartNode().hasRelationship(Direction.INCOMING, DEF._))
						return false;
		
		return true;
	};

	protected Set<Relationship> getExpected(PFlow pf) {
		Set<Relationship> thes = null;
		
        for (Relationship r : pf.getOPNode().getRelationships(OUTGOING)) {
            Statement st = Statements.relationshipType(r);
            if (st instanceof Predicate) {
                try {
                	if (thes == null)
                		thes = ((Predicate) st).getExpected(pf, r);
                	else {
                		Set<Relationship> set = ((Predicate) st).getExpected(pf, r);
                		if (set != null)
                			thes.addAll( set );
                	}

                } catch (Throwable t) {
                    //XXX: report
                    t.printStackTrace();
                }
            }
        }
        return thes;
	}
}