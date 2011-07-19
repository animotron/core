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

import static org.animotron.graph.RelationshipTypes.REF;

import org.animotron.manipulator.OnQuestion;
import org.animotron.manipulator.PFlow;
import org.animotron.operator.AbstractOperator;
import org.animotron.operator.Evaluable;
import org.animotron.operator.relation.HAVE;
import org.neo4j.graphdb.Path;
import org.neo4j.graphdb.Relationship;

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
		return new OnQuestion() {
			@Override
			public void onMessage(final PFlow pf) {
				System.out.println("SELF '"+name(pf.getOP())+"' op = "+pf.getOP());
				
				System.out.println("path = "+pf.getFlowPath());
				
				Path path = pf.getFlowPath();
				
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
					Relationship res = GET._.get(ref.getEndNode(), name(pf.getOP()));
					
					if (res != null)
						pf.sendAnswer(createResultInMemory(pf.getOPNode(), res));
				} else if (searchHave == 2) {
					//the instance self in have
					Relationship res = GET._.get(pf.getStartNode(), name(pf.getOP()));
					
					if (res != null)
						pf.sendAnswer(createResultInMemory(pf.getOPNode(), res));
					
				} else
					;//XXX: error???
				
				pf.done();
			}
		};
	}
	
}
