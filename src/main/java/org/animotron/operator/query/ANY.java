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
import org.animotron.io.PipedOutputObjectStream;
import org.animotron.operator.AbstarctOperator;
import org.animotron.operator.Query;
import org.animotron.operator.Utils;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.Transaction;

/**
 * Query operator 'ANY'.
 * 
 * Retrun 'all' or first 'perfect' USE
 * 
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 */
public class ANY extends AbstarctOperator implements Query {
	
	private static final ANY INSTANCE = new ANY();
	public static ANY getInstance() { return INSTANCE; }
	
	private ANY() { super("any", "animo/query/any"); }
	
	@Override
	public void eval(Relationship op, PipedOutputObjectStream out, boolean isLast) throws IOException {
//		PipedInputObjectStream in = new PipedInputObjectStream();

//		if (!isLast)
//			Calculator.eval(op, new PipedOutputObjectStream(in));
		
		Transaction tx = op.getGraphDatabase().beginTx();
		try {
			Node node = op.getEndNode();

			//check, maybe, result was already calculated
			if (!Utils.results(node, out)) {

				//go to 'THE' node
				Node the = Utils.getByREF(node); 
	
				//get 'THE' relation - ???
				Relationship res = node.createRelationshipTo(the, RelationshipTypes.RESULT);
	
				out.write(res);
			}

			tx.success();
		} finally {
			tx.finish();
		}
		out.close();
	}
}
