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
import static org.animotron.graph.RelationshipTypes.RESULT;

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
            System.out.println("SELF '"+ reference(pf.getOP())+"' op = "+pf.getOP());

            Relationship op = pf.getOP();
            
            Path path = pf.getFlowPath();
            System.out.println("path = "+path);

            Relationship res = selfByTraversal(pf, op, op.getStartNode(), (String) reference(op));
			if (res != null) {
				System.out.println("FOUND by traversal");
				pf.sendAnswer(op, res);
				pf.done();
				return;
			}

            Relationship lastContext = pf.getLastContext();

            short searchHave = 1;

            Relationship ref = null;
            for (Relationship step : path.relationships()) {
                if (step.isType(REF)) {
                    ref = step;
                    searchHave = 0;
                } else if (searchHave == 1 && step.isType(HAVE._)) {
                    searchHave = 2;
                }

                if (step == lastContext)
                    break;
            }

            if (ref != null) {
                //reference in processing flow
                res = GET._.getBySELF(pf, ref.getEndNode(), (String) reference(pf.getOP()));

                if (res != null)
                    pf.sendAnswer(op, createResult(pf.getLastContext(), pf.getOPNode(), res, HAVE._));

            } else if (searchHave == 2) {
                //the instance self in have
                res = GET._.getBySELF(pf, pf.getStartNode(), (String) reference(pf.getOP()));

                if (res != null)
                    pf.sendAnswer(op, createResult(pf.getLastContext(), pf.getOPNode(), res, HAVE._));
                    //TODO Why don't create persistent relationship?

            } else
                ;//XXX: error???

            pf.done();
        }
    };

	private Relationship selfByTraversal(PFlow pf, final Relationship start_op, final Node eNode, final String name) {
		
		Node node = Utils.getByREF(start_op.getEndNode());

		//System.out.println("start_op = "+start_op+" eNode = "+eNode+" sNode = "+node);

		boolean underUSE = false;
		for (Path path : getUSEtravers(pf.getStartOP()).traverse(node)) {
			System.out.println(" path = "+path);
			for (Relationship p : path.relationships()) {
				if (p.isType(USE._)) {
					node = p.getEndNode();
					underUSE = true;
					break;
				}
			}
		}

		TraversalDescription td = getDirectedTravers(eNode);
		
//		for (Path path : td.traverse(node)) {
//			System.out.println("path = "+path);
//		}
		
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

				if (r.isType(IS._) && name.equals(IS._.reference(r))) {
					foundIS = true;
					
				} else if (r.isType(HAVE._) && (name.equals(HAVE._.reference(r)) || foundIS)) {
					thisResult = r;
					thisDeep++;
				
				} else if (r.isType(IC._) && (name.equals(IC._.reference(r)) || foundIS)) {
					if (foundIS) {
						//store
						final Node sN = eNode;
						final Node eN = r.getEndNode();

						thisResult = AnimoGraph.execute(new GraphOperation<Relationship>() {
							@Override
							public Relationship execute() {
								Relationship res = sN.createRelationshipTo(eN, HAVE._);
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
