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
package org.animotron.operator.query;

import org.animotron.Properties;
import org.animotron.Statement;
import org.animotron.Statements;
import org.animotron.manipulator.PFlow;
import org.animotron.operator.*;
import org.animotron.operator.relation.IS;
import org.animotron.operator.relation.USE;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Path;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.traversal.Evaluation;
import org.neo4j.graphdb.traversal.Evaluators;
import org.neo4j.graphdb.traversal.TraversalDescription;
import org.neo4j.kernel.Traversal;
import org.neo4j.kernel.Uniqueness;

import static org.neo4j.graphdb.Direction.INCOMING;
import static org.neo4j.graphdb.Direction.OUTGOING;
import static org.neo4j.graphdb.traversal.Evaluation.*;

/**
 * Abstract Query
 * 
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 */
public abstract class AbstractQuery extends AbstractOperator implements Cachable, Evaluable, Query {

    public AbstractQuery(String prefix, String uri) {
        super(prefix, uri);
    }

    protected TraversalDescription td_IS =
        Traversal.description().
            breadthFirst().
            relationships(IS._.relationshipType(), INCOMING ).
            evaluator(Evaluators.excludeStartPosition());

    protected boolean filtering(PFlow pf, Node node) {
        for (Relationship r : pf.getOPNode().getRelationships(OUTGOING)) {
            Statement st = Statements.relationshipType(r);
            if (st instanceof Predicate) {
                try {
                    if (!((Predicate) st).filter(pf, pf.getStartOP(), r, node))
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

	protected Relationship getThe(Node node) {
		return THE._.get(Properties.NAME.get(node));
	}
	
	protected TraversalDescription getUSEtravers(final Relationship end) {

		return Traversal.description().depthFirst().
		uniqueness(Uniqueness.RELATIONSHIP_PATH).
		evaluator(new org.neo4j.graphdb.traversal.Evaluator(){
			@Override
			public Evaluation evaluate(Path path) {
				if (path.length() > 0) {
					Relationship r = path.lastRelationship(); 
					if (r.getStartNode().equals(path.endNode())) {
						if (r.equals(end)) {
							return INCLUDE_AND_PRUNE;
						} 

						String rType = r.getType().name();
						
						if (path.length() == 1 && !(rType.equals(USE._.rType) || rType.equals(IS._.rType)) ) {
							return EXCLUDE_AND_PRUNE;
						}
						return EXCLUDE_AND_CONTINUE;
					
					//Allow IS->USE<-IS->...
					} if (path.length() > 2 && r.getType().name().equals(IS._.rType)) {
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