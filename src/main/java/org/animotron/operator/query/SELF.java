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

import org.animotron.graph.AnimoGraph;
import org.animotron.graph.GraphOperation;
import org.animotron.graph.RelationshipTypes;
import org.animotron.manipulator.OnQuestion;
import org.animotron.manipulator.PFlow;
import org.animotron.operator.AbstractOperator;
import org.animotron.operator.Evaluable;
import org.animotron.operator.IC;
import org.animotron.operator.relation.HAVE;
import org.animotron.operator.relation.IS;
import org.neo4j.graphdb.Direction;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Path;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.traversal.Evaluation;
import org.neo4j.graphdb.traversal.TraversalDescription;
import org.neo4j.kernel.Traversal;
import org.neo4j.kernel.Uniqueness;

import static org.animotron.graph.RelationshipTypes.REF;
import static org.neo4j.graphdb.traversal.Evaluation.EXCLUDE_AND_CONTINUE;
import static org.neo4j.graphdb.traversal.Evaluation.EXCLUDE_AND_PRUNE;
import static org.neo4j.graphdb.traversal.Evaluation.INCLUDE_AND_PRUNE;

/**
 * Query operator 'self'.
 * 
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 */
public class SELF extends AbstractOperator implements Evaluable {
	
	public static final SELF _ = new SELF();
	
	private SELF() { super("self", "animo/query/self"); }

	@Override
	public OnQuestion onCalcQuestion() {
        return question;
    }

	private OnQuestion question = new OnQuestion() {
        @Override
        public void onMessage(final PFlow pf) {
            System.out.println("SELF '"+name(pf.getOP())+"' op = "+pf.getOP());

            Relationship op = pf.getOP();
            
			Relationship res = selfByTraversal(op, op.getStartNode(), name(op));
			if (res != null) {
				pf.sendAnswer(res);
				pf.done();
				return;
			}

            Path path = pf.getFlowPath();
            System.out.println("path = "+path);
            

            Relationship lastContext = pf.getLastContext();

            short searchHave = 1;

            Relationship ref = null;
            for (Relationship step : path.relationships()) {
                if (REF.name().equals(step.getType().name())) {
                    ref = step;
                    searchHave = 0;
                } else if (searchHave == 1 && HAVE._.relationshipType().name().equals(step.getType().name())) {
                    searchHave = 2;
                }

                if (step == lastContext)
                    break;
            }

            if (ref != null) {
                //reference in processing flow
                res = GET._.get(ref.getEndNode(), name(pf.getOP()));

                if (res != null)
                    pf.sendAnswer(createResultInMemory(pf.getOPNode(), res));

            } else if (searchHave == 2) {
                //the instance self in have
                res = GET._.get(pf.getStartNode(), name(pf.getOP()));

                if (res != null)
                    pf.sendAnswer(createResultInMemory(pf.getOPNode(), res));
                    //TODO Why don't create persistent relationship?

            } else
                ;//XXX: error???

            pf.done();
        }
    };

	private Relationship selfByTraversal(final Relationship start_op, final Node eNode, final String name) {
		
		TraversalDescription td =
			Traversal.description().depthFirst().
			uniqueness(Uniqueness.RELATIONSHIP_PATH).
			evaluator(new org.neo4j.graphdb.traversal.Evaluator(){
				@Override
				public Evaluation evaluate(Path path) {
					//System.out.println("path = "+path);
					if (path.length() > 0) {
						Relationship r = path.lastRelationship(); 
						if (r.getStartNode().equals(path.endNode())) {
							if (r.getStartNode().equals(eNode)) {
								return INCLUDE_AND_PRUNE;
							} 
							return EXCLUDE_AND_CONTINUE;	
						} 
						return EXCLUDE_AND_PRUNE;
					}
					return EXCLUDE_AND_CONTINUE;
				}
			});
		
		System.out.println("start_op = "+start_op+" eNode = "+eNode);
		
		Node node = start_op.getEndNode().getSingleRelationship(RelationshipTypes.REF, Direction.OUTGOING).getEndNode();
		for (Path path : td.traverse(node)) {
			System.out.println("path = "+path);
		}
		
		int deep = Integer.MAX_VALUE;
		Relationship result = null;

		Relationship thisResult = null;
		int thisDeep = 0;
		
		for (Path path : td.traverse(node)) {
			
			boolean foundIS = false;
			thisDeep = 0;
			for (Relationship r : path.relationships()) {
				if (thisDeep > 0) {
					thisDeep++;
					continue;
				}
				String type = r.getType().name();
				
				if (type.equals(IS._.relationshipType().name()) && name.equals(IS._.name(r))) {
					foundIS = true;
					
				} else if (type.equals(HAVE._.relationshipType().name()) && (name.equals(HAVE._.name(r)) || foundIS)) {
					thisResult = r;
					thisDeep++;
				
				} else if (type.equals(IC._.relationshipType().name()) && (name.equals(IC._.name(r)) || foundIS)) {
					if (foundIS) {
						//store
						final Node sN = eNode;
						final Node eN = r.getEndNode();

						thisResult = AnimoGraph.execute(new GraphOperation<Relationship>() {
							@Override
							public Relationship execute() {
								Relationship res = sN.createRelationshipTo(eN, HAVE._.relationshipType());
								//RID.set(res, r.getId());
								return res;
							}
						});
						thisDeep++;
					}
				}
			}
			
			if (thisDeep < deep)
				result = thisResult;
		}
		
		return result;
	}
}
