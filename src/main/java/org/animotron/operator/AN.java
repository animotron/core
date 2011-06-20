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
package org.animotron.operator;

import static org.animotron.graph.AnimoGraph.beginTx;
import static org.animotron.graph.AnimoGraph.finishTx;
import static org.neo4j.graphdb.Direction.OUTGOING;

import java.io.IOException;

import org.animotron.Catcher;
import org.animotron.graph.RelationshipTypes;
import org.animotron.io.PipedOutputObjectStream;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.Transaction;

/**
 * Operation 'AN'. Direct reference to 'the' instance.
 * 
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 */
public class AN extends AbstarctOperator implements Reference, Evaluable, Cachable {
	
	public static final AN _ = new AN();
	
	private AN() { super("an", "animo/reference"); }
	
	@Override
	public void eval(Relationship op, PipedOutputObjectStream out, boolean isLast, Catcher catcher) throws IOException {
//		PipedInputObjectStream in = new PipedInputObjectStream();
//		if (!isLast)
//			Evaluator._.execute(op, new PipedOutputObjectStream(in));
		
		Transaction tx = beginTx();
		try {
		
			Node node = op.getEndNode();

			Relationship res = node.getSingleRelationship(
				RelationshipTypes.REF, OUTGOING
			);
			
			out.write(res);

			tx.success();
		} finally {
			finishTx(tx);
		}
		out.close();
	}
}
