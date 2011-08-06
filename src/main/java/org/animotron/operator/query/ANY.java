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

import org.animotron.manipulator.OnQuestion;
import org.animotron.manipulator.PFlow;
import org.animotron.operator.Utils;
import org.animotron.operator.relation.IS;
import org.animotron.operator.relation.USE;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Path;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.traversal.Evaluation;
import org.neo4j.graphdb.traversal.TraversalDescription;
import org.neo4j.kernel.Traversal;
import org.neo4j.kernel.Uniqueness;

import static org.animotron.graph.RelationshipTypes.REF;
import static org.neo4j.graphdb.Direction.OUTGOING;
import static org.neo4j.graphdb.traversal.Evaluation.EXCLUDE_AND_CONTINUE;
import static org.neo4j.graphdb.traversal.Evaluation.EXCLUDE_AND_PRUNE;
import static org.neo4j.graphdb.traversal.Evaluation.INCLUDE_AND_PRUNE;

/**
 * Query operator 'ANY'.
 * 
 * Return 'all' or first 'perfect' USE
 * 
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 */
public class ANY extends AbstractQuery {
	
	public static final ANY _ = new ANY();
	
	private ANY() { super("any", "animo/query/any"); }

	public OnQuestion onCalcQuestion() {
        return question;
    }

	private OnQuestion question = new OnQuestion() {
        @Override
        public void onMessage(final PFlow pf) {
            final Node n = pf.getOP().getEndNode();
            Relationship ref = n.getSingleRelationship( REF, OUTGOING );

            Node node = ref.getEndNode();

            final Relationship end = pf.getStartOP(); 

			TraversalDescription td = Traversal.description().depthFirst().
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
						} 
						return EXCLUDE_AND_PRUNE;
					}
					return EXCLUDE_AND_CONTINUE;
				}
			});
			
			System.out.println("ANY **************************");
			for (Path path : td.traverse(Utils.getByREF(pf.getOP().getEndNode()))) {
				System.out.println(" path = "+path);
				for (Relationship p : path.relationships()) {
					if (p.getType().name().equals(USE._.rType)) {
						node = p.getEndNode();
					}
				}
			}

			if (filtering(pf, node)) {
				pf.sendAnswer( createResultInMemory( n, getThe(node) ) );
			} else {
	            for (Relationship tdR : td_IS.traverse(node).relationships()) {
                    System.out.println("ANY get next "+tdR+" ["+tdR.getStartNode()+"]");
                    Node res = tdR.getStartNode();
                    if (filtering(pf, res)) {

                        pf.sendAnswer( createResultInMemory( n, getThe(res) ) );
                        break;
                    }
                }
			}
            pf.done();
        }

    };


}
