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

import java.util.Iterator;

import org.jetlang.channels.Channel;
import org.jetlang.channels.MemoryChannel;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class PFlow {
	
	private final Manipulator m;

	public final Channel<Relationship> answer = new MemoryChannel<Relationship>();
	public final Channel<PFlow> question = new MemoryChannel<PFlow>();
	public final Channel<Void> stop = new MemoryChannel<Void>();
	
	protected PFlow parent = null;
	private Relationship op = null;
	private Node opNode = null;
	
	private PFlow(Manipulator m) {
		this.m = m;
	};
	
	public PFlow(Manipulator m, Relationship op) {
		parent = new PFlow(m);
		this.m = m;
		this.op = op;
	}

	public PFlow(Manipulator m, Node opNode) {
		parent = new PFlow(m);
		this.m = m;
		this.opNode = opNode;
	}

	public PFlow(PFlow parent, Relationship op) {
		this.parent = parent;
		this.m = parent.m;
		this.op = op;
	}

	public PFlow(PFlow parent, Node opNode) {
		this.parent = parent;
		this.m = parent.m;
		this.opNode = opNode;
	}
	
	public PFlow getParent() {
		return parent;
	}

	public Relationship getOP() {
		return op;
	}
	
	public Node getOPNode() {
		if (opNode != null)
			return opNode;
		
		return op.getEndNode();
	}

	protected void setOPNode(Node opNode) {
		this.opNode = opNode;
		this.op = null;
	}

	public void sendAnswer(Relationship r) {
		if (parent == null) {
			System.out.println("WORNG - no parent");
			//System.out.println("send answer to "+answer+" (this = "+this+")");
			answer.publish(r);
		} else {
			//System.out.println("send answer to "+parent.answer+" (parent = "+parent+")");
			parent.answer.publish(r);
		}
	}

	public void done() {
		if (parent == null) answer.publish(null);
		else parent.answer.publish(null);
	}

	public Manipulator getManipulator() {
		return m;
	}
	
	public Iterable<PFlow> stack() {
		return new Iterable<PFlow>() {
			@Override
			public Iterator<PFlow> iterator() {
				return iterator();
			}
		};
	}
	
	public PFlowStack iterator() {
		return new PFlowStack();
	}
	
	private class PFlowStack implements Iterator<PFlow> {
		
		private PFlow pos = parent;
		
		@Override
		public boolean hasNext() {
			return pos.parent != null;
		}

		@Override
		public PFlow next() {
			pos = pos.parent;
			return pos;
		}

		@Override
		public void remove() {
			// TODO Auto-generated method stub
		}

		
	}
	
}