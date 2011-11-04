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
import org.animotron.statement.Statement;
import org.animotron.statement.Statements;
import org.animotron.statement.operator.*;
import org.animotron.statement.relation.IS;
import org.animotron.statement.relation.USE;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Path;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.traversal.Evaluation;
import org.neo4j.graphdb.traversal.Evaluators;
import org.neo4j.graphdb.traversal.TraversalDescription;
import org.neo4j.kernel.Traversal;
import org.neo4j.kernel.Uniqueness;

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

    public AbstractQuery(String name) {
        super(name);
    }

    protected TraversalDescription td_IS =
        Traversal.description().
            breadthFirst().
            relationships(IS._, INCOMING ).
            evaluator(Evaluators.excludeStartPosition());

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
    
	protected Set<Node>[] getUSEs(Node start, Relationship end) {

    	Set<Node> uses = null;
    	Set<Node> directed = null;
    	
    	TraversalDescription td = getUSEtravers(end);
    	
    	int deep = 0;
    	int deepest = 0;
    	Node deepestNode = null;

		for (Path path : td.traverse(start)) {
			//System.out.println(" path = "+path);
			
			Node lastNode = path.startNode();
			boolean isDirected = true;
			deep = 0;
			for (Relationship p : path.relationships()) {
				if (p.isType(USE._)) {
					
					if (isDirected) {
						if (directed == null) directed = new FastSet<Node>();
						directed.add( p.getEndNode() );
						
						if (deepest < deep) {
							deepestNode = p.getEndNode();
							deepest = deep;
						}
						
					} else {
						if (uses == null) uses = new FastSet<Node>();
						uses.add( p.getEndNode() );
					}
					break;
				
				} else if (!(p.isType(IS._))) {
					break;
				}
				
				if (isDirected && !lastNode.equals(p.getEndNode()))
					isDirected = false;
				else {
					deep++;
					lastNode = p.getStartNode();
				}
			}
		}

		Set<Node> deepestSet = null;
		if (deepestNode != null) {
			deepestSet = new FastSet<Node>();
			deepestSet.add(deepestNode);			
		}
		return new Set[] {directed, uses, deepestSet};
    }

	protected Relationship getThe(Node node) {
		return THE._.get((String) Properties.NAME.get(node));
	}
	
	protected TraversalDescription getIntersectionChecktravers(final Set<Node> mustHave) {

		return Traversal.description().depthFirst().
		uniqueness(Uniqueness.RELATIONSHIP_PATH).
		evaluator(new org.neo4j.graphdb.traversal.Evaluator(){
			@Override
			public Evaluation evaluate(Path path) {
				if (path.length() > 0) {
					
					//System.out.println(" "+path);
					
					Relationship r = path.lastRelationship();
					if (!r.isType(IS._))
						return EXCLUDE_AND_PRUNE;
						
					Node sNode = r.getEndNode();
					if (!sNode.equals(path.endNode()))
						return EXCLUDE_AND_PRUNE;
					
					mustHave.remove(sNode);
					if (mustHave.size() == 0)
						return INCLUDE_AND_PRUNE;

				}
				return EXCLUDE_AND_CONTINUE;
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

						if (path.length() == 1 && !(r.isType(USE._) || r.isType(IS._))) {
							return EXCLUDE_AND_PRUNE;
						}
						return EXCLUDE_AND_CONTINUE;
					
					//Allow IS<-USE<-IS->... and IS<-IS->...<-USE 
					} if (path.length() > 1 && r.isType(IS._)) {
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
}