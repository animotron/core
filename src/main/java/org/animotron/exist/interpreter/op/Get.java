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
package org.animotron.exist.interpreter.op;

import java.io.IOException;

import org.animotron.exist.index.AnimoIndex;
import org.animotron.exist.index.RelationshipTypes;
import org.animotron.exist.interpreter.Calculator;
import org.animotron.io.PipedInputObjectStream;
import org.animotron.io.PipedOutputObjectStream;
import org.neo4j.graphdb.Direction;
import org.neo4j.graphdb.GraphDatabaseService;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Path;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.Transaction;
import org.neo4j.graphdb.traversal.Evaluation;
import org.neo4j.graphdb.traversal.Evaluator;
import org.neo4j.graphdb.traversal.TraversalDescription;
import org.neo4j.helpers.Predicate;
import org.neo4j.kernel.Traversal;

/**
 * Operation 'get'. Return 'have' relations on provided context.
 * 
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 */
public class Get {

	private static TraversalDescription td_res = 
		Traversal.description().
			breadthFirst().
			relationships(RelationshipTypes.RESULT, Direction.OUTGOING );
			//.evaluator(Evaluators.excludeStartPosition());

	private static TraversalDescription td_eval = 
		Traversal.description().
			breadthFirst().
			relationships(RelationshipTypes.HAVE, Direction.OUTGOING );
			//.evaluator(Evaluators.excludeStartPosition());

	public static void eval(Relationship op, PipedOutputObjectStream out, boolean isLast) throws IOException {
		
		//check, maybe, result was already calculated
		boolean haveResult = false;
		Node node = op.getEndNode();
		for (Relationship res : td_res.traverse(node).relationships()) {
			
			out.write(res);
			
			haveResult = true;
		}
		
		if (haveResult) {
			//close out pipe?
			return;
		}
		
		//no pre-calculated result, calculate it
		PipedInputObjectStream in = new PipedInputObjectStream();

		Calculator.eval(op, new PipedOutputObjectStream(in));
		
		Object n; 
		while ((n = in.read()) != null) {
			if (n instanceof Relationship) {

				GraphDatabaseService graphdb = node.getGraphDatabase();
				Transaction tx = graphdb.beginTx();
				try {
					for (Relationship r : td_eval.traverse(((Relationship) n).getEndNode()).relationships()) {
						Relationship res = node.createRelationshipTo(r.getEndNode(), RelationshipTypes.RESULT);
						out.write(res);
					}
					tx.success();
				} finally {
					tx.finish();
				}
			}
		}
	}
}
