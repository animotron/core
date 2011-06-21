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
package org.animotron.walker;

import static org.neo4j.graphdb.Direction.OUTGOING;

import org.animotron.Executor;
import org.animotron.Statement;
import org.animotron.Statements;
import static org.animotron.graph.AnimoGraph.*;
import org.animotron.manipulator.SimpleManipulator;
import org.animotron.manipulator.StatementManipulator;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.Transaction;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * 
 */
public abstract class Walker {
	
	private static Runnable run (final Relationship op, SimpleManipulator m) {
		return new Operation() {
			@Override
			public void execute() {
				m.go(op);
			}
		};
	}

	private static Runnable run (final Relationship op, StatementManipulator m) {
		Statement statement = Statements.relationshipType(op.getType());
		if (m.canGo(statement))
			return new Operation() {
				@Override
				public void execute() {
					m.go(statement, op);
				}
			};
		else 
			return null;
	}

	public static void start(final Relationship op, SimpleManipulator m) {
		Executor.getFiber().execute(run(op, m));
	}
	
	public static void start(final Relationship op, StatementManipulator m) {
		Executor.getFiber().execute(run(op, m));
	}
	
	public static void start(final Node op, StatementManipulator m) {
		for (Relationship r : op.getRelationships(OUTGOING)) {
			start(r, m);
		}
	}
	
	public static void start(final Node op, SimpleManipulator m) {
		for (Relationship r : op.getRelationships(OUTGOING)) {
			start(r, m);
		}
	}
	
	private abstract class Operation implements Runnable {
		
		public void run() {
			Transaction tx = beginTx();
			try {
				execute();
				tx.success();
			} finally {
				finishTx(tx);
			}
		}

		public abstract void execute();
		
	}
	
}
