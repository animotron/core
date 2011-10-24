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
package org.animotron.statement.relation;

import org.animotron.graph.AnimoGraph;
import org.animotron.graph.GraphOperation;
import org.animotron.manipulator.OnQuestion;
import org.animotron.manipulator.PFlow;
import org.animotron.statement.operator.Prepare;
import org.jetlang.channels.Subscribable;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Path;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.traversal.Evaluation;
import org.neo4j.graphdb.traversal.Evaluator;
import org.neo4j.graphdb.traversal.TraversalDescription;
import org.neo4j.graphdb.traversal.Traverser;
import org.neo4j.kernel.Traversal;
import org.neo4j.kernel.Uniqueness;

import static org.animotron.graph.RelationshipTypes.TOP;
import static org.neo4j.graphdb.Direction.INCOMING;
import static org.neo4j.graphdb.Direction.OUTGOING;
import static org.neo4j.graphdb.traversal.Evaluation.*;

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
		super("is");
		TD = Traversal.description()
			.depthFirst()
			.uniqueness(Uniqueness.RELATIONSHIP_PATH)
			.relationships(this, OUTGOING);
	}
	
	private OnQuestion question = new OnQuestion() {
		
		@Override
		public void onMessage(PFlow pf) {
			
			Relationship op = pf.getOP();
			final Node start = op.getStartNode();
			
			Traverser t = TD.evaluator(new Evaluator() {
                @Override
                public Evaluation evaluate(Path path) {
                    if (path.length() == 0)
                        return EXCLUDE_AND_CONTINUE;
                    if (path.lastRelationship().isType(_))
                        return Evaluation.EXCLUDE_AND_CONTINUE;
                    if (path.endNode().equals(start))
                        return INCLUDE_AND_PRUNE;
                    return EXCLUDE_AND_PRUNE;
                }
            }).traverse(start);
			
			if (t.iterator().hasNext()) {
				AnimoGraph.execute(
					new GraphOperation<Void>() {
						@Override
						public Void execute() {
							for (Relationship r : start.getRelationships(TOP, INCOMING)) {
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