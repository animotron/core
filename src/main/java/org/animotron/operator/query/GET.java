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

import org.animotron.graph.RelationshipTypes;
import org.animotron.interpreter.Calculator;
import org.animotron.io.PipedInputObjectStream;
import org.animotron.io.PipedOutputObjectStream;
import org.animotron.operator.AbstarctOperator;
import org.animotron.operator.Cachable;
import org.animotron.operator.Evaluable;
import org.animotron.operator.Query;
import org.animotron.operator.Utils;
import org.animotron.operator.relation.HAVE;
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

	private static final GET _ = new GET();
	public static GET getInstance() { return _; }
	
	private GET() { super("get", "animo/query/extract"); }

	private static TraversalDescription td_eval = 
		Traversal.description().
			breadthFirst().
			relationships(HAVE.getInstance().relationshipType(), Direction.OUTGOING );
			//.evaluator(Evaluators.excludeStartPosition());

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
				
				Object n; 
				while ((n = in.read()) != null) {
					if (n instanceof Relationship) {
		
						for (Relationship r : td_eval.traverse(((Relationship) n).getEndNode()).relationships()) {
							
							System.out.println("GET eval = "+r);

							Relationship res = node.createRelationshipTo(r.getEndNode(), RelationshipTypes.RESULT);
							out.write(res);
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
