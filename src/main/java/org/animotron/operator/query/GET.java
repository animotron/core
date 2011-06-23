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

import static org.neo4j.graphdb.Direction.OUTGOING;

import org.animotron.Executor;
import org.animotron.Statement;
import org.animotron.Statements;
import org.animotron.manipulator.PFlow;
import org.animotron.operator.AbstarctOperator;
import org.animotron.operator.Cachable;
import org.animotron.operator.Evaluable;
import org.animotron.operator.IC;
import org.animotron.operator.Query;
import org.animotron.operator.Utils;
import org.animotron.operator.relation.HAVE;
import org.animotron.operator.relation.IS;
import org.jetlang.core.Callback;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.traversal.TraversalDescription;
import org.neo4j.kernel.Traversal;

/**
 * Query operator 'Get'. Return 'have' relations on provided context.
 * 
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 */
public class GET extends AbstarctOperator implements Evaluable, Query, Cachable {

	public static final GET _ = new GET();
	
	private GET() { super("get", "animo/query/extract"); }

	private static TraversalDescription td_eval = 
		Traversal.description().
			breadthFirst().
			relationships(HAVE._.relationshipType(), OUTGOING );
			//.evaluator(Evaluators.excludeStartPosition());

	private static TraversalDescription td_eval_ic = 
		Traversal.description().
			breadthFirst().
			relationships(IS._.relationshipType(), OUTGOING ).
			relationships(IC._.relationshipType(), OUTGOING );

	@Override
	public void eval(Relationship op, PFlow ch, boolean isLast) {
	}
	
	Callback<PFlow> onQuestion = new Callback<PFlow>() {

		@Override
		public void onMessage(final PFlow pf) {
			final Relationship op = pf.getOP();
			
			final Node node = op.getEndNode();
			
			//check, maybe, result was already calculated
			if (!Utils.results(node, pf)) {
				//no pre-calculated result, calculate it
				
				final String name = name(op);
				
//				PFlow nextPF = new PFlow();
				
				Callback<Relationship> onContext = new Callback<Relationship>() {
					@Override
					public void onMessage(Relationship context) {
						Relationship res = get(context.getEndNode(), name);
						
						if (res != null)
							pf.answer.publish(createResult(node, res));
					}
					
				};
				
//				nextPF.answer.subscribe(Executor.getFiber(), onContext);
			}
		}
		
	};

	public Relationship get(final Node context, final String name) {
		
		//search local 'HAVE'
		for (Relationship tdR : td_eval.traverse(context).relationships()) {
			
			System.out.println("GET get = "+tdR);
			
			if (name.equals(name(tdR)))
				return tdR;
		}
		
		//search 'IC' by 'IS' topology
		for (Relationship tdR : td_eval_ic.traverse(context).relationships()) {
			
			Statement st = Statements.relationshipType( tdR.getType() );
			if (st instanceof IS) {
				System.out.println("GET IC -> IS "+tdR);
				
			} else if (st instanceof IC) {
				System.out.print("GET IC -> "+tdR);
				
				if (name.equals(name(tdR))) {
					System.out.println(" MATCH");
					return tdR;
				}
				System.out.println();
			}
		}
		
		return null;
	}
}
