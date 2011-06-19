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

import static org.animotron.graph.AnimoGraph.beginTx;
import static org.animotron.graph.AnimoGraph.finishTx;
import static org.neo4j.graphdb.Direction.OUTGOING;

import java.io.IOException;
import java.util.Iterator;

import org.animotron.io.PipedOutputObjectStream;
import org.animotron.manipulator.Catcher;
import org.animotron.manipulator.Executor;
import org.animotron.manipulator.Manipulator;
import org.animotron.manipulator.Startable;
import org.animotron.marker.Marker;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.PropertyContainer;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.Transaction;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * 
 */
public abstract class Walker implements Runnable, Startable {

	private Manipulator m;
	private PropertyContainer op;
	private PipedOutputObjectStream out;
	private Marker marker;

	public Walker(Manipulator m, PropertyContainer op, PipedOutputObjectStream out, Marker marker) {
		this.m = m;
		this.op = op;
		this.out = out;
		this.marker = marker;
		if (marker !=null)
			marker.mark(op instanceof Node ? (Node) op : ((Relationship) op).getEndNode());
	}
	
	@Override
	public final void run() {
		Catcher catcher = new Catcher(); 
		
		Transaction tx = beginTx();
		try {
			if (op instanceof Node)
				go((Node) op, out, catcher);
			else
				go((Relationship) op, out, catcher);
			
			reset();
			
			tx.success();
			out.close();
		} catch (IOException e) {
			e.printStackTrace();
			catcher = null;
			tx.failure();
			
		} finally {
			finishTx(tx);
		}
		
		if (catcher != null)
			catcher.run();
	}
	
	private void go(Relationship op, PipedOutputObjectStream ot, Catcher catcher) throws IOException {
		go(op.getEndNode(), ot, catcher);
	}

	private final void go(Node node, PipedOutputObjectStream ot, Catcher catcher) throws IOException {
		//System.out.println("Walk node = " + node);
		Iterator<Relationship> it = node.getRelationships(OUTGOING).iterator();
		while (it.hasNext()) {
			Relationship r = it.next();
			go(r, ot, catcher, isLast(it));
		}
	}

	protected abstract void go(Relationship op, PipedOutputObjectStream ot, Catcher catcher, boolean isLast) throws IOException;

	private boolean isLast(Iterator<?> it) {
		return !it.hasNext();
	}
	
	protected final Manipulator getManipulator(){
		return m;
	}
	
	public final void start() {
		Executor.execute(this);
	}
	
	public void reset() {
		if (marker != null)
			marker.drop();	
	}

}
