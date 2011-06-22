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

import java.util.Iterator;

import org.animotron.marker.Marker;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.Transaction;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public abstract class Manipulator {

	protected abstract void execute (Relationship op, PFlow ch, Marker marker, boolean isLast);

	public Marker marker(){
		return null;
	}
	
	public void shutdown() {
		//TODO: make shutdown
	}
	
	public final PFlow execute(Node op) {
		PFlow ch = new PFlow();
		execute (op, ch);
		return ch;
	}
	
	public final PFlow execute(Relationship op) {
		PFlow pf = new PFlow();
		execute(op, pf);
		
		pf.question.publish(new PFlow(pf, op));
		
		return pf;
	}
	
	public final void execute(Node op, PFlow ch) {
		execute (op, ch, null);
	}
	
	public final void execute(Relationship op, PFlow ch) {
		execute (op, ch, null);
	}
	
	public final PFlow mark(Node op) {
		PFlow ch = new PFlow();
		mark (op, ch);
		return ch;
	}
	
	public final PFlow mark(Relationship op) {
		PFlow ch = new PFlow();
		mark (op, ch);
		return ch;
	}
	
	public final void mark(Node op, PFlow ch) {
		execute (op, ch, marker());
	}
	
	public final void mark(Relationship op, PFlow ch) {
		execute (op, ch, marker());
	}
	
	private void execute(Relationship op, PFlow ch, Marker marker) {
		execute(op.getEndNode(), ch, marker);
	}
	
	private void execute(Node op, PFlow ch, Marker marker) {
		Iterator<Relationship> it = op.getRelationships(OUTGOING).iterator();
		while (it.hasNext()) {
			Relationship r = it.next();
			execute(r, ch, marker, !it.hasNext());
		}
	}
	
	protected abstract class Operation implements Runnable {
		
		private Marker marker;
		
		protected Operation (Marker marker) {
			this.marker = marker;
		}
		
		@Override
		public void run() {
			Transaction tx = beginTx();
			try {
				execute();
				tx.success();
			} finally {
				finishTx(tx);
			}
		} 

		protected abstract void execute();
		
	}

	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
}