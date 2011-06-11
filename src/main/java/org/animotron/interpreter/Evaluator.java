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
package org.animotron.interpreter;

import java.io.IOException;
import java.util.Iterator;

import org.animotron.Statement;
import org.animotron.Statements;
import org.animotron.io.PipedInputObjectStream;
import org.animotron.io.PipedOutputObjectStream;
import org.animotron.operator.AbstarctOperator;
import org.neo4j.graphdb.Direction;
import org.neo4j.graphdb.GraphDatabaseService;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.RelationshipType;
import org.neo4j.graphdb.Transaction;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
class Evaluator extends Walker {
	
	public Evaluator(Relationship op, PipedOutputObjectStream out) {
		super(op, out);
	}

	@Override
	protected void eval(Relationship op, PipedOutputObjectStream ot) throws IOException {
		System.out.println("Evaluator op = "+op);
		
		GraphDatabaseService graphdb = op.getGraphDatabase();
		Transaction tx = graphdb.beginTx();
		try {
			Relationship r = null;
			
			Node node = op.getEndNode();
			System.out.println("Evaluator node = "+node);
			Iterator<Relationship> it = node.getRelationships(Direction.OUTGOING).iterator();
			while (it.hasNext()) {
				
				r = it.next();
				RelationshipType type = r.getType();
				
				System.out.println(type.name());
				
				Statement s = Statements.relationshipType(type);

				if (s == null)
					;//???
				else if (s instanceof AbstarctOperator) {
					AbstarctOperator oper = (AbstarctOperator) s;

					PipedInputObjectStream in = new PipedInputObjectStream();

					oper.eval(r, new PipedOutputObjectStream(in), isLast(it));
					
					Object n; 
					while ((n = in.read()) != null) {
						ot.write(n);
					} 
					
				} else
					ot.write(r);
			}

			tx.success();
		
		} catch (IOException e) {
			e.printStackTrace();
			//XXX: terminate?
		} finally {
			tx.finish();
		}

		ot.close();
	}
	
}