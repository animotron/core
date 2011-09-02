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

import org.animotron.graph.AnimoGraph;
import org.animotron.graph.GraphOperation;
import org.animotron.manipulator.OnQuestion;
import org.animotron.manipulator.PFlow;
import org.animotron.statement.operator.IC;
import org.animotron.statement.operator.Utils;
import org.animotron.statement.relation.HAVE;
import org.animotron.statement.relation.IS;
import org.animotron.statement.relation.USE;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Path;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.traversal.TraversalDescription;

import static org.animotron.graph.RelationshipTypes.REF;

/**
 * Query operator 'self'.
 * 
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 */
public class SELF extends AbstractQuery {
	
	public static final SELF _ = new SELF();
	
	private SELF() { super("self"); }

	@Override
	public OnQuestion onCalcQuestion() {
        return question;
    }

	private OnQuestion question = new OnQuestion() {
        @Override
        public void onMessage(final PFlow pf) {
            System.out.println("SELF '"+name(pf.getOP())+"' op = "+pf.getOP());

            Relationship op = pf.getOP();
            
			Relationship res = selfByTraversal(pf, op, op.getStartNode(), name(op));
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

	private Relationship selfByTraversal(PFlow pf, final Relationship start_op, final Node eNode, final String name) {
		
		Node node = Utils.getByREF(start_op.getEndNode());

		System.out.println("start_op = "+start_op+" eNode = "+eNode+" sNode = "+node);

		boolean underUSE = false;
		for (Path path : getUSEtravers(pf.getStartOP()).traverse(node)) {
			System.out.println(" path = "+path);
			for (Relationship p : path.relationships()) {
				if (p.getType().name().equals(USE._.rType)) {
					node = p.getEndNode();
					underUSE = true;
					break;
				}
			}
		}

		TraversalDescription td = getDirectedTravers(eNode);
		
		for (Path path : td.traverse(node)) {
			System.out.println("path = "+path);
		}
		
		int deep = Integer.MAX_VALUE;
		Relationship result = null;

		Relationship thisResult = null;
		int thisDeep = 0;
		
		for (Path path : td.traverse(node)) {
			
			boolean foundIS = underUSE;
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
