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
import org.animotron.Properties;
import org.animotron.manipulator.PFlow;
import org.animotron.manipulator.QCAVector;
import org.animotron.statement.Statement;
import org.animotron.statement.Statements;
import org.animotron.statement.operator.*;
import org.animotron.statement.relation.SHALL;
import org.animotron.statement.relation.USE;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Path;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.traversal.Evaluation;
import org.neo4j.graphdb.traversal.Evaluator;
import org.neo4j.graphdb.traversal.TraversalDescription;
import org.neo4j.kernel.Traversal;
import org.neo4j.kernel.Uniqueness;

import java.util.List;
import java.util.Set;

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
    				
    				if (path.length() % 2 == 0 && path.lastRelationship().isType(AN._))
    					return INCLUDE_AND_CONTINUE;
    				
    				if (path.length() % 2 == 1 && !path.lastRelationship().isType(REF._))
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
        				
        				if (path.length() % 2 == 0 && path.lastRelationship().isType(AN._))
        					return INCLUDE_AND_CONTINUE;
        				
        				if (path.length() % 2 == 1 && !path.lastRelationship().isType(REF._))
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
        				
        				if (path.length() % 2 == 0 && !path.lastRelationship().isType(AN._))
        					return EXCLUDE_AND_PRUNE;
        				
        				if (path.length() % 2 == 1 && path.lastRelationship().isType(REF._))
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
        					
        				if (path.length() == 1 && path.lastRelationship().isType(REF._))
    						return EXCLUDE_AND_CONTINUE;
        					
        				if (path.length() == 2 && path.lastRelationship().isType(THE._))
    						return INCLUDE_AND_PRUNE;
        				
    					return EXCLUDE_AND_PRUNE;
        			}
        		});


    protected boolean filtering(PFlow pf, Node node, Set<Node> uses) {
    	if (uses != null) {
    		//check intersection
    		TraversalDescription td = getIntersectionChecktravers(new FastSet<Node>(uses));
    		if (!td.traverse(node).iterator().hasNext())
    			return false;
    	}
    	
        for (Relationship r : pf.getOPNode().getRelationships(OUTGOING)) {
            Statement st = Statements.relationshipType(r);
            if (st instanceof Predicate) {
                try {
                    if (!((Predicate) st).filter(pf, r, node))
                        return false;
                } catch (Exception e) {
                    //XXX: report
                    e.printStackTrace();
                    return false;
                }
            }
        }
        return true;
    }
    
    private void searchForUSE(Set<Node> uses, final List<QCAVector> vectors) {
    	if (vectors == null) return;
    	
		for (QCAVector vector : vectors) {
			searchForUSE(uses, vector);
		}
    }

    private void checkVectorForUSE(Set<Node> uses, QCAVector vector) {
    	if (vector == null)
    		return;

    	System.out.println(" cheking "+vector);
    	
		if (vector.getQuestion() != null)
    		for (Relationship r : vector.getQuestion().getEndNode().getRelationships(OUTGOING, USE._)) {
    			uses.add(r.getEndNode());
    			//System.out.println(" + "+r.getEndNode());
    		}

		if (vector.getUnrelaxedAnswer() != null)
    		for (Relationship r : vector.getUnrelaxedAnswer().getEndNode().getRelationships(OUTGOING, USE._)) {
    			uses.add(r.getEndNode());
    			//System.out.println(" + "+r.getEndNode());
    		}

		if (vector.getAnswers() != null)
			for (Relationship a : vector.getAnswers())
	    		for (Relationship r : a.getEndNode().getRelationships(OUTGOING, USE._)) {
	    			uses.add(r.getEndNode());
	    			//System.out.println(" + "+r.getEndNode());
	    		}
    }
    
    private void searchForUSE(Set<Node> uses, QCAVector vector) {
    	//System.out.println("searchForUSE "+vector);
    	QCAVector prev = vector.getPrecedingSibling();
    	
    	checkVectorForUSE(uses, prev); 
    	
    	checkVectorForUSE(uses, vector); 

		searchForUSE(uses, vector.getContext());
    }
    
	protected Set<Node>[] getUSEs(PFlow pf, Node theNode) {

    	final Set<Node> allUses = new FastSet<Node>();
    	Set<Node> directed = null;
    	Set<Node> deepestSet = null;//new FastSet<Node>();
    	
    	for (QCAVector v : pf.getPFlowPath()) {
			searchForUSE(allUses, v);
    	}

    	if (allUses.isEmpty()) {
    		return new Set[] {directed, allUses, deepestSet};
    	}
    		
    	final Set<Node> uses = new FastSet<Node>();

    	TraversalDescription trav = td.
				relationships(AN._, INCOMING).
				relationships(REF._, INCOMING).
				relationships(THE._, INCOMING).
		evaluator(new IntersectionSearcher(){
			@Override
			public Evaluation evaluate(Path path) {
				//System.out.println(" "+path);
				return _evaluate_(path, allUses, uses);
			}
		});
    	
    	for (Path path : trav.traverse(theNode)) {
    		//System.out.println(" * "+path);
    	}
		if (allUses.contains(theNode))
			uses.add(theNode);
    	
		return new Set[] {directed, uses, deepestSet};
    }

//	protected Set<Node>[] getUSEs(Node start, Relationship end) {
//
//    	Set<Node> uses = null;
//    	Set<Node> directed = null;
//    	
//    	TraversalDescription td = getUSEtravers(end);
//    	
//    	int deep = 0;
//    	int deepest = 0;
//    	Node deepestNode = null;
//
//		for (Path path : td.traverse(start)) {
//			//System.out.println(" path = "+path);
//			
//			Node lastNode = path.startNode();
//			boolean isDirected = true;
//			deep = 0;
//			for (Relationship p : path.relationships()) {
//				if (p.isType(USE._)) {
//					
//					if (isDirected) {
//						if (directed == null) directed = new FastSet<Node>();
//						directed.add( p.getEndNode() );
//						
//						if (deepest < deep) {
//							deepestNode = p.getEndNode();
//							deepest = deep;
//						}
//						
//					} else {
//						if (uses == null) uses = new FastSet<Node>();
//						uses.add( p.getEndNode() );
//					}
//					break;
//				
//				} else if (!(p.isType(AN._))) {
//					break;
//				}
//				
//				if (isDirected && !lastNode.equals(p.getEndNode()))
//					isDirected = false;
//				else {
//					deep++;
//					lastNode = p.getStartNode();
//				}
//			}
//		}
//
//		Set<Node> deepestSet = null;
//		if (deepestNode != null) {
//			deepestSet = new FastSet<Node>();
//			deepestSet.add(deepestNode);			
//		}
//		return new Set[] {directed, uses, deepestSet};
//    }

	protected Relationship getThe(Node node) {
		return THE._.get((String) Properties.NAME.get(node));
	}
	
	protected TraversalDescription getIntersectionChecktravers(final Set<Node> mustHave) {

		return Traversal.description().depthFirst().
		uniqueness(Uniqueness.RELATIONSHIP_PATH).
		evaluator(new org.neo4j.graphdb.traversal.Evaluator(){
			@Override
			public Evaluation evaluate(Path path) {
				//System.out.println(" "+path);

				Node sNode;
				if (path.length() == 0) {
					sNode = path.startNode();
				} else {
					
					Relationship firstR = firstRelationsip(path);
					Relationship lastR = path.lastRelationship();
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
					}

					sNode = lastR.getEndNode();
					if (!sNode.equals(path.endNode()))
						return EXCLUDE_AND_PRUNE;
				}
				
				mustHave.remove(sNode);
				if (mustHave.size() == 0)
					return INCLUDE_AND_PRUNE;

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
				} else if (r.isType(AN._)) {
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

		public Evaluation _evaluate_(Path path, final Set<Node> targets, final Set<Node> intersection) {
			if (path.length() < 2)
				return EXCLUDE_AND_CONTINUE;
			
			Relationship r = path.lastRelationship();
			
			if (r.isType(THE._)) {
				Node n = r.getEndNode(); 
				if (targets.contains(n)) {
					intersection.add(n);
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
						//System.out.println("  "+path);
						return _evaluate_(path, targets, intersection);
					}
				});

		    	for (Path p : trav.traverse(r.getStartNode())) {
		    		//System.out.println(" ** "+p);
		    	}

			} else if (path.length() % 2 == 1)
				if (!r.isType(REF._))
					return EXCLUDE_AND_PRUNE;
				else {
					Node n = r.getEndNode(); 
					if (targets.contains(n)) {
						intersection.add(n);
					}
				}
				

			return EXCLUDE_AND_CONTINUE;
		}
	};

	abstract class DownIntersectionSearcher implements Evaluator {

		public Evaluation _evaluate_(Path path, Set<Node> targets, Set<Node> intersection) {
			if (path.length() < 2)
				return EXCLUDE_AND_CONTINUE;
			
			Relationship r = path.lastRelationship();
			
			if (r.isType(THE._)) {
				Node n = r.getEndNode(); 
				if (targets.contains(n)) {
					intersection.add(n);
				}
			
			} else if (path.length() % 2 == 1) {
				if (!r.isType(AN._))
					return EXCLUDE_AND_PRUNE;
				
			} else if (path.length() % 2 == 0)
				if (!r.isType(REF._))
					return EXCLUDE_AND_PRUNE;
				else {
					Node n = r.getEndNode(); 
					if (targets.contains(n)) {
						intersection.add(n);
					}
				}
				

			return EXCLUDE_AND_CONTINUE;
		}
	};

}