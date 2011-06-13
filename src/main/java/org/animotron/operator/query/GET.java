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

import static org.animotron.graph.AnimoGraph.beginTx;
import static org.animotron.graph.AnimoGraph.finishTx;
import static org.animotron.graph.RelationshipTypes.RESULT;
import static org.neo4j.graphdb.Direction.OUTGOING;

import java.io.IOException;

import org.animotron.Statement;
import org.animotron.Statements;
import org.animotron.interpreter.Calculator;
import org.animotron.io.PipedOutputObjectStream;
import org.animotron.operator.AbstarctOperator;
import org.animotron.operator.Cachable;
import org.animotron.operator.Evaluable;
import org.animotron.operator.IC;
import org.animotron.operator.Query;
import org.animotron.operator.Utils;
import org.animotron.operator.relation.HAVE;
import org.animotron.operator.relation.IS;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.Transaction;
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
	public void eval(Relationship op, PipedOutputObjectStream out, boolean isLast) throws IOException {
		
		Transaction tx = beginTx();
		try {
			Node node = op.getEndNode();
			
			//check, maybe, result was already calculated
			if (!Utils.results(node, out)) {
				//no pre-calculated result, calculate it
				
				String name = name(op);
				
				for (Object n : Calculator.eval(op)) {
					if (n instanceof IOException) {
						throw (IOException)n;
						
					} else if (n instanceof Relationship) {
						
						final Relationship r = (Relationship)n;
						
						boolean found = false;
						if (RESULT.equals(r.getType().name())) {
							System.out.println("GET get-result "+r);
							continue;
						} 
						
						for (Relationship tdR : td_eval.traverse(r.getEndNode()).relationships()) {
							
							System.out.println("GET eval = "+tdR);
							
							if (name.equals(name(tdR))) {
								out.write(createResult(node, tdR));
								
								found = true;
							}
						}
						
						if (!found) {
							for (Relationship tdR : td_eval_ic.traverse(r.getEndNode()).relationships()) {
								
								Statement st = Statements.relationshipType( tdR.getType() );
								if (st instanceof IS) {
									System.out.println("GET IC -> IS "+tdR);
									
								} else if (st instanceof IC) {
									System.out.print("GET IC -> "+tdR);
									
									if (name.equals(name(tdR))) {
										System.out.print(" MATCH");
										out.write(createResult(node, tdR));
									}

									System.out.println();
								}
							}
						}
					}
				}
			}
			tx.success();
		} finally {
			finishTx(tx);
		}
		
		out.close();
	}
}
