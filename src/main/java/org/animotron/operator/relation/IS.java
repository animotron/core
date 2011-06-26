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
package org.animotron.operator.relation;

import static org.neo4j.graphdb.Direction.INCOMING;
import static org.neo4j.graphdb.Direction.OUTGOING;

import org.animotron.graph.AnimoGraph;
import org.animotron.graph.GraphOperation;
import org.animotron.graph.RelationshipTypes;
import org.animotron.manipulator.OnQuestion;
import org.animotron.manipulator.PFlow;
import org.animotron.operator.Prepare;
import org.animotron.operator.Relation;
import org.jetlang.channels.Subscribable;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Path;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.traversal.Evaluators;
import org.neo4j.graphdb.traversal.TraversalDescription;
import org.neo4j.graphdb.traversal.Traverser;
import org.neo4j.helpers.Predicate;
import org.neo4j.kernel.Traversal;
import org.neo4j.kernel.Uniqueness;

/**
 * Operator 'IS'.
 * 
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 */
public class IS extends Relation implements Prepare {
	
	public static final IS _ = new IS();
	
	private static TraversalDescription TD;
	
	private IS() { 
		super("is", "animo/relation/is"); 
		TD = Traversal.description()
			.depthFirst()
			.uniqueness(Uniqueness.RELATIONSHIP_PATH)
			.relationships(relationshipType(), OUTGOING)
			.evaluator(Evaluators.fromDepth(1));
	}
	
	private Predicate<Path> predicate(final Node node) {
		return new Predicate<Path>() {
            public boolean accept(Path pos) {
            	if (pos.endNode().equals(node)){
            		return false;
            	} else if (pos.endNode().hasRelationship(relationshipType(), OUTGOING)) {
            		return false;
            	} else {
            		return true;
            	}
            }
        };
	}
	
	private OnQuestion question = new OnQuestion() {
		
		@Override
		public void onMessage(PFlow pf) {
			
			Relationship op = pf.getOP();
			final Node start = op.getStartNode();
			
			@SuppressWarnings("deprecation")
			Traverser t = TD.filter(predicate(start)).traverse(start);
			
			if (t.iterator().hasNext()) {
				AnimoGraph.execute(
					new GraphOperation<Void>() {
						@Override
						public Void execute() {
							for (Relationship r : start.getRelationships(RelationshipTypes.TOP, INCOMING)) {
								r.delete();
							}
							return null;
						}
					}
				);
			}
			
		}

	};

	@Override
	public Subscribable<PFlow> onPrepareQuestion() {
		return question;
	}

}