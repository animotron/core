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

import java.io.IOException;

import org.animotron.Properties;
import org.animotron.Statement;
import org.animotron.Statements;
import org.animotron.graph.RelationshipTypes;
import org.animotron.interpreter.Calculator;
import org.animotron.io.*;
import org.animotron.operator.AbstarctOperator;
import org.animotron.operator.Cachable;
import org.animotron.operator.Evaluable;
import org.animotron.operator.IC;
import org.animotron.operator.Query;
import org.animotron.operator.Utils;
import org.animotron.operator.relation.*;
import org.neo4j.graphdb.Direction;
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
			relationships(HAVE._.relationshipType(), Direction.OUTGOING );
			//.evaluator(Evaluators.excludeStartPosition());

	private static TraversalDescription td_eval_ic = 
		Traversal.description().
			breadthFirst().
			relationships(IS._.relationshipType(), Direction.OUTGOING ).
			relationships(IC._.relationshipType(), Direction.OUTGOING );

	@Override
	public void eval(Relationship op, PipedOutputObjectStream out, boolean isLast) throws IOException {
		
		Transaction tx = op.getGraphDatabase().beginTx();
		try {
			Node node = op.getEndNode();
			
			//check, maybe, result was already calculated
			if (!Utils.results(node, out)) {
				//no pre-calculated result, calculate it
				PipedInputObjectStream in = new PipedInputObjectStream();
		
				Calculator.eval(op, new PipedOutputObjectStream(in));
				
				String name = name(op);
				
				Object n; 
				while ((n = in.read()) != null) {
					if (n instanceof Relationship) {
						boolean found = false;
						for (Relationship r : td_eval.traverse(((Relationship) n).getEndNode()).relationships()) {
							
							System.out.println("GET eval = "+r);

							Relationship res = node.createRelationshipTo(r.getEndNode(), RelationshipTypes.RESULT);
							out.write(res);
							
							found = true;
						}
						
						if (!found) {
							for (Relationship r : td_eval_ic.traverse(((Relationship) n).getEndNode()).relationships()) {
								
								Statement st = Statements.relationshipType( r.getType() );
								if (st instanceof IS) {
									System.out.println("GET IC -> IS "+r);
								} else if (st instanceof IC) {
									System.out.println("GET IC -> FOUND "+r);
									
									Relationship res = node.createRelationshipTo(r.getEndNode(), RelationshipTypes.RESULT);
									//store to relationsip arrow 
									Properties.RID.set(res, r.getId());
									
									out.write(res);
								}

							}
						}
					}
				}
			}
			tx.success();
		} finally {
			tx.finish();
		}
		
		out.close();
	}
}
