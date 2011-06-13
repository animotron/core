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
import static org.animotron.graph.RelationshipTypes.REF;
import static org.animotron.graph.RelationshipTypes.RESULT;
import static org.neo4j.graphdb.Direction.*;

import java.io.IOException;

import org.animotron.graph.InMemoryRelationship;
import org.animotron.interpreter.Calculator;
import org.animotron.interpreter.ResultOnContext;
import org.animotron.io.PipedInputObjectStream;
import org.animotron.io.PipedOutputObjectStream;
import org.animotron.operator.AbstarctOperator;
import org.animotron.operator.Cachable;
import org.animotron.operator.Evaluable;
import org.animotron.operator.relation.IS;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.Transaction;
import org.neo4j.graphdb.traversal.TraversalDescription;
import org.neo4j.kernel.Traversal;

/**
 * Query operator 'ANY'.
 * 
 * Retrun 'all' or first 'perfect' USE
 * 
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 */
public class ANY extends AbstarctOperator implements Cachable, Evaluable {
	
	public static final ANY _ = new ANY();
	
	private ANY() { super("any", "animo/query/any"); }
	
	@Override
	public void eval(Relationship op, PipedOutputObjectStream ot, boolean isLast) throws IOException {
		
		PipedInputObjectStream in = new PipedInputObjectStream();
		PipedOutputObjectStream out = new PipedOutputObjectStream(in);
		Calculator.filter(op, out);
		
		Transaction tx = beginTx();
		try {
			Node node = op.getEndNode();
			Relationship ref = node.getSingleRelationship( REF, OUTGOING );
			
			if (out.filter(ref)) {
				ot.write(new ResultANY( ref ));
			} else {
				
				for (Relationship tdR : td_eval.traverse(ref.getEndNode()).relationships()) {
					System.out.println("ANY get next "+tdR);
					if (out.filter(tdR)) {
						ot.write(new ResultANY( tdR ));
						break;
					}
				}
			}

			tx.success();
		} finally {
			finishTx(tx);
		}
		ot.close();
	}
	
	class ResultANY extends InMemoryRelationship implements ResultOnContext {

		protected ResultANY(Relationship r) {
			super(r.getStartNode(), r.getEndNode(), RESULT);
		}
		
//		public String toString() {
//			return "RESULT:ANY";
//		}
	}
	
	private static TraversalDescription td_eval = 
		Traversal.description().
			breadthFirst().
			relationships(IS._.relationshipType(), INCOMING );

}
