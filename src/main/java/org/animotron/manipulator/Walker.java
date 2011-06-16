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
package org.animotron.manipulator;

import static org.animotron.graph.AnimoGraph.beginTx;
import static org.animotron.graph.AnimoGraph.finishTx;
import static org.neo4j.graphdb.Direction.OUTGOING;

import java.io.IOException;
import java.util.Iterator;

import org.animotron.Statement;
import org.animotron.Statements;
import org.animotron.graph.RelationshipTypes;
import org.animotron.io.PipedInputObjectStream;
import org.animotron.io.PipedOutputObjectStream;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.PropertyContainer;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.RelationshipType;
import org.neo4j.graphdb.Transaction;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * 
 */
class Walker<T extends Manipulator> implements Runnable {

	private T m;
	private PropertyContainer op;
	private PipedOutputObjectStream out;

	public Walker(T m, PropertyContainer op, PipedOutputObjectStream out) {
		this.m = m;
		this.op = op;
		this.out = out;
	}

	@Override
	public void run() {
		try {
			if (op instanceof Node)
				go((Node) op, out);
			else
				go((Relationship) op, out);
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	protected void go(Relationship op, PipedOutputObjectStream ot) throws IOException {
		//System.out.println("Walk op = " + op);
		go(op.getEndNode(), ot);
	}

	protected void go(Node node, PipedOutputObjectStream ot) throws IOException {

		//System.out.println("Walk node = " + node);

		Transaction tx = beginTx();
		try {
			Relationship r = null;

			Iterator<Relationship> it = node.getRelationships(OUTGOING).iterator();
			while (it.hasNext()) {

				r = it.next();
				RelationshipType type = r.getType();

				//System.out.println(type.name());

				Statement s = Statements.relationshipType(type);

				if (m.canGo(s)) {

					PipedInputObjectStream in = null;
					PipedOutputObjectStream out = ot;

					if (m.isPiped()) {
						in = new PipedInputObjectStream();
						out = new PipedOutputObjectStream(in);
					}

					m.go(s, r, out, isLast(it));

					if (m.isPiped()) {
						for (Object n : in) {
							ot.write(n);
						}
					}
					
				//XXX:find better solution
				} else if (type.name().equals(RelationshipTypes.REF.name())) {
					//ignore
				} else {
					System.out.println("Not evaled " + r);
					//ot.write(r);
				}
			}
			tx.success();

		} catch (IOException e) {
			e.printStackTrace();
			ot.write(e);
		} finally {
			finishTx(tx);
		}

		ot.close();
	}

	protected boolean isLast(Iterator<?> it) {
		return !it.hasNext();
	}
}