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

import javolution.util.FastSet;
import javolution.util.FastTable;

import org.animotron.graph.Properties;
import org.animotron.graph.index.Order;
import org.animotron.manipulator.PFlow;
import org.animotron.manipulator.QCAVector;
import org.animotron.statement.Statement;
import org.animotron.statement.Statements;
import org.animotron.statement.operator.*;
import org.animotron.statement.relation.SHALL;
import org.animotron.statement.relation.USE;
import org.animotron.statement.relation.WEAK_USE;
import org.neo4j.graphdb.Direction;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Path;
import org.neo4j.graphdb.Relationship;
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
    				//System.out.println(path);
    				
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
                //evaluator(Evaluators.excludeStartPosition()).
                evaluator(new org.neo4j.graphdb.traversal.Evaluator(){
        			@Override
        			public Evaluation evaluate(Path path) {
        				if (path.endNode().getId() == 0)
        					return EXCLUDE_AND_PRUNE;
        				
        				//System.out.println(path);
        				
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
    
    protected static TraversalDescription td_IS_down =
            Traversal.description().
                breadthFirst().
                relationships(REF._, OUTGOING ).
                relationships(AN._, OUTGOING ).
                //evaluator(Evaluators.excludeStartPosition()).
                evaluator(new org.neo4j.graphdb.traversal.Evaluator(){
        			@Override
        			public Evaluation evaluate(Path path) {
        				//System.out.println(" "+path);
        				
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
        					
        				if (path.length() == 2 && r.isType(THE._))
    						return INCLUDE_AND_PRUNE;
        				
    					return EXCLUDE_AND_PRUNE;
        			}
        		});


    protected boolean filtering(final PFlow pf, final Relationship ref, final Set<Node> uses, final Set<Node> weaks) {
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

	    		TraversalDescription td = getIntersectionChecktravers(uSet, wSet, weaks.size());
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

    private void collectUSEs(final Set<Node> uses, final Set<Node> weaks, final Node node) {
		for (Relationship r : node.getRelationships(OUTGOING, USE._)) {
			for (Relationship rr : r.getEndNode().getRelationships(OUTGOING, REF._))
				if (!FREEZE.has(rr))
					uses.add(rr.getEndNode());
		}
		for (Relationship r : node.getRelationships(OUTGOING, WEAK_USE._)) {
			for (Relationship rr : r.getEndNode().getRelationships(OUTGOING, REF._))
				if (!FREEZE.has(rr))
					weaks.add(rr.getEndNode());
		}
    }

    private void checkVectorForUSE(final Set<Node> uses, final Set<Node> weaks, final QCAVector vector, final Set<QCAVector> visitred) {
    	if (vector == null)
    		return;

		if (vector.getQuestion() != null)
			collectUSEs(uses, weaks, vector.getQuestion().getEndNode());

		if (vector.getUnrelaxedAnswer() != null)
			collectUSEs(uses, weaks, vector.getUnrelaxedAnswer().getEndNode());

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
			
	    	prev = prev.getPrecedingSibling();
    	}
    	
    }
    
	protected void getUSEs(PFlow pf, Node theNode, final Set<Node> uses, final Set<Node> weaks, final Set<Path> directed) {

    	final FastSet<QCAVector> visited = FastSet.newInstance();
    	final FastSet<Node> allUses = FastSet.newInstance();
    	final FastSet<Node> allWeaks = FastSet.newInstance();
    	try {
			searchForUSE(allUses, allWeaks, pf.getVector(), visited);
			
			//System.out.println("allUses "+allUses);
			//System.out.println("allWeaks "+allWeaks);
	
	    	if (allUses.isEmpty()) return;
	    		
	    	TraversalDescription trav = td.breadthFirst().
					relationships(AN._, INCOMING).
					relationships(REF._, INCOMING).
					relationships(THE._, INCOMING).
			evaluator(new IntersectionSearcher(){
				@Override
				public Evaluation evaluate(Path path) {
					//System.out.println(" "+path);
					return _evaluate_(path, allUses, allWeaks, uses, weaks, directed);
				}
			});
	    	
	    	for (Path path : trav.traverse(theNode)) {
	    		//System.out.println(" * use * "+path);
	    	}
	
	    	if (allUses.contains(theNode))
				uses.add(theNode);

	    	if (allWeaks.contains(theNode))
	    		weaks.add(theNode);
	    	
    	} finally {
    		FastSet.recycle(allUses);
    		FastSet.recycle(allWeaks);
    		FastSet.recycle(visited);
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

	protected Relationship getThe(Node node) {
		try {
			return THE._.get((String) Properties.NAME.get(node));
		} catch (Throwable t) {
			return null;
		}
	}
	
	protected TraversalDescription getIntersectionChecktravers(final Set<Node> mustHave, final Set<Node> shouldHave, final int was) {

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
					
					Relationship firstR = firstRelationsip(path);
					Relationship lastR = path.lastRelationship();
					if (lastR.isType(REF._) && FREEZE.has(lastR))
						return EXCLUDE_AND_PRUNE;
					
					if (firstR.isType(AN._)) {
	    				if (path.length() % 2 == 1 && !lastR.isType(AN._))
	    					return EXCLUDE_AND_PRUNE;
	    				
	    				if (path.length() % 2 == 0 && !lastR.isType(REF._))
	    					return EXCLUDE_AND_PRUNE;
					
					} else if (firstR.isType(REF._)) {
	    				if (path.length() % 2 == 0 && !lastR.isType(AN._))
	    					return EXCLUDE_AND_PRUNE;
	    				
	    				if (path.length() % 2 == 1 && !lastR.isType(REF._))
	    					return EXCLUDE_AND_PRUNE;
					} else {
						return EXCLUDE_AND_PRUNE;
					}

					sNode = lastR.getEndNode();
					if (!sNode.equals(path.endNode()))
						return EXCLUDE_AND_PRUNE;
				}
				
				if (shouldHave != null) shouldHave.remove(sNode);
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
				//System.out.println(" "+path);
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

		public Evaluation _evaluate_(Path path, Set<Node> targets) { //, RelationshipType type
			//System.out.println(path);
			
			if (path.length() == 0)
				return EXCLUDE_AND_CONTINUE;
			
			Relationship r = path.lastRelationship();
			if (r.isType(REF._) && FREEZE.has(r))
				return EXCLUDE_AND_PRUNE;
			
			if (r.isType(SHALL._)) {
				if (path.length() < 2)
					return EXCLUDE_AND_PRUNE;

				for (Path p : td_IS_down.traverse(r.getEndNode())) {
					if (targets.contains(p.lastRelationship().getEndNode()))
						return INCLUDE_AND_PRUNE;
				}
				//for (QCAVector rr : Utils.getByREF(null, r))
				//	if (targets.contains(rr.getAnswer().getEndNode()))
				//		return INCLUDE_AND_PRUNE;
				
				return EXCLUDE_AND_PRUNE;
			}

			if (path.length() == 1) {
				if (r.isType(REF._) && targets.contains(r.getEndNode()))
					return INCLUDE_AND_PRUNE;

//				if (r.isType(type))
//					return EXCLUDE_AND_CONTINUE;
					
				return EXCLUDE_AND_CONTINUE;
				
			} else if (path.length() >= 2) {
				if (r.isType(REF._)) {
					Node node = r.getEndNode();
					if (targets.contains(node)) 
						return INCLUDE_AND_PRUNE;

					return EXCLUDE_AND_CONTINUE;
				
				//XXX: check direction!
				} else if (r.isType(AN._) || r.isType(ANY._)) {
					if (r.getEndNode().equals(path.endNode())) {
						return EXCLUDE_AND_CONTINUE;
					}
					return EXCLUDE_AND_PRUNE;
				}
			}
			return EXCLUDE_AND_PRUNE;
		}
	}

	abstract class IntersectionSearcher implements Evaluator {

		public Evaluation _evaluate_(Path path, final Set<Node> targets, final Set<Node> weakTargets, final Set<Node> intersection, final Set<Node> weakIntersection, final Set<Path> directed) {
			if (path.length() < 2)
				return EXCLUDE_AND_CONTINUE;
			
			final Relationship r = path.lastRelationship();
			
			if (r.isType(REF._) && FREEZE.has(r))
				return EXCLUDE_AND_PRUNE;

			if (r.isType(THE._)) {
				Node n = r.getEndNode(); 
				if (targets.contains(n)) {
					if (debugUSE) 
						System.out.println("THE->"+path);
						
					intersection.add(n);

					directed.add(path);//n
				
				} else if (weakTargets.contains(n)) {
					if (debugUSE) 
						System.out.println("[weak] THE->"+path);
						
					weakIntersection.add(n);

					directed.add(path);//n
				}

			} else if (path.length() % 2 == 0) {
				if (!r.isType(AN._))
					return EXCLUDE_AND_PRUNE;
				
		    	TraversalDescription trav = td.
						relationships(AN._, OUTGOING).
						relationships(REF._, OUTGOING).
						relationships(THE._, OUTGOING).
				evaluator(new DownIntersectionSearcher(){
					@Override
					public Evaluation evaluate(Path path) {
						//XXX: make this work
//						if (path.length() == 1 && path.lastRelationship().equals(r)) {
//							System.out.println(" - "+path+" - ");
//							return EXCLUDE_AND_PRUNE;
//						}
						
						//System.out.println(" - "+path);
						return _evaluate_(path, targets, weakTargets, intersection, weakIntersection);
					}
				});

				if (debugUSE) System.out.println(" - "+path);
		    	for (Path p : trav.traverse(r.getStartNode())) {

		    		//System.out.println(path);
		    		//System.out.println(" ** "+p);

					directed.add(path);//r.getStartNode()
		    	}
		    	
		    	if (Utils.haveContext(r.getEndNode()))
		    		return EXCLUDE_AND_PRUNE;

			} else if (path.length() % 2 == 1)
				if (!r.isType(REF._))
					return EXCLUDE_AND_PRUNE;
				else {
					Node n = r.getEndNode(); 
					if (targets.contains(n)) {
						//System.out.println("->"+path);

						intersection.add(n);

						directed.add(path);//n
					} else if (weakTargets.contains(n)) {
						//System.out.println("->"+path);

						weakIntersection.add(n);

						directed.add(path);//n
					}
				}
				

			return EXCLUDE_AND_CONTINUE;
		}
	};

	abstract class DownIntersectionSearcher implements Evaluator {

		public Evaluation _evaluate_(final Path path, final Set<Node> targets, final Set<Node> weakTargets, final Set<Node> intersection, final Set<Node> weakIntersection) {
			if (path.length() < 2)
				return EXCLUDE_AND_CONTINUE;
			
			Relationship r = path.lastRelationship();
			if (r.isType(REF._) && FREEZE.has(r))
				return EXCLUDE_AND_PRUNE;
				
			
			if (r.isType(THE._)) {
				Node n = r.getEndNode(); 
				
				if (targets.contains(n)) {
					intersection.add(n);
					return INCLUDE_AND_PRUNE;
				
				} else if (weakTargets.contains(n)) {
					weakIntersection.add(n);
					return INCLUDE_AND_PRUNE;
					
				}
				return EXCLUDE_AND_PRUNE;
			
			} else if (path.length() % 2 == 1) {
				if (!r.isType(AN._))
					return EXCLUDE_AND_PRUNE;
				
			} else if (path.length() % 2 == 0)
				if (!r.isType(REF._))
					return EXCLUDE_AND_PRUNE;
				else {
					Node n = r.getEndNode(); 
					if (targets.contains(n)) {
						if (debugUSE) System.out.println(" -> "+path);
						intersection.add(n);
						return INCLUDE_AND_PRUNE;
					
					} else if (weakTargets.contains(n)) {
						if (debugUSE) System.out.println("[weak] -> "+path);
						weakIntersection.add(n);
						return INCLUDE_AND_PRUNE;
						
					}
					return EXCLUDE_AND_PRUNE;
				}
				

			return EXCLUDE_AND_CONTINUE;
		}
	};

	abstract class ISearcher implements Evaluator {

		public Evaluation _evaluate_(final Path path, final Set<Node> targets) {
			//System.out.println(path);
			
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
					if (rr.getStartNode().hasRelationship(Direction.INCOMING, THE._))
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