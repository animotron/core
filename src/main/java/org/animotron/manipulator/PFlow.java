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

import org.animotron.exception.AnimoException;
import org.animotron.graph.RelationshipTypes;
import org.animotron.statement.Statement;
import org.animotron.statement.Statements;
import org.animotron.statement.operator.AN;
import org.animotron.statement.operator.Reference;
import org.animotron.statement.relation.IS;
import org.animotron.statement.relation.USE;
import org.jetlang.channels.Channel;
import org.jetlang.channels.MemoryChannel;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Path;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.RelationshipType;
import org.neo4j.graphdb.traversal.Evaluation;
import org.neo4j.graphdb.traversal.Evaluator;
import org.neo4j.graphdb.traversal.TraversalDescription;
import org.neo4j.kernel.Traversal;
import org.neo4j.kernel.Uniqueness;

import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.Vector;
import java.util.concurrent.CountDownLatch;

import static org.animotron.graph.RelationshipTypes.REF;
import static org.neo4j.graphdb.Direction.OUTGOING;
import static org.neo4j.graphdb.traversal.Evaluation.*;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class PFlow {
	
	private final Manipulator m;

	public final Channel<Relationship> answer = new MemoryChannel<Relationship>();
	public final Channel<PFlow> question = new MemoryChannel<PFlow>();
	public final Channel<Throwable> stop = new MemoryChannel<Throwable>();
	
	protected final PFlow parent;
	private Relationship op = null;
	private Node opNode = null;
	
	private Vector<Relationship> path = new Vector<Relationship>();
	
	public PFlow(Manipulator m, Relationship op) {
		parent = null;
		this.m = m;
		this.op = op;
		
		path.add(op);
	}

	public PFlow(PFlow parent, Relationship op) {
		System.out.println("new PFlow this = "+PFlowToString(this)+" parent = "+PFlowToString(parent));
		this.parent = parent;
		this.m = parent.m;
		
		//XXX: maybe, clone faster?
		path.addAll(parent.path);
		
		//RelationshipType type = op.getType();
		//Statement s = Statements.relationshipType(type);
		//if (s instanceof Reference) {
		if (!path.firstElement().equals(op))
			path.insertElementAt(op, 0);
		
		//} else if (RelationshipTypes.RESULT.name().equals(type.name())) {
		//	path.insertElementAt(op, 0);
		//}
		
		this.op = op;
	}

	private String PFlowToString(PFlow pf) {
		return pf.toString().substring(pf.toString().indexOf("@"));
	}

	public PFlow(PFlow parent, Node opNode) {
		this.parent = parent;
		this.m = parent.m;
		
		//XXX: maybe, clone faster?
		path.addAll(parent.path);

		this.opNode = opNode;
	}
	
	public PFlow getParent() {
		return parent;
	}

	public Relationship getOP() {
		return op;
	}

	public Relationship getStartOP() {
		return path.lastElement();
	}
	
	public Node getStartNode() {
		return path.lastElement().getEndNode();
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

	public void sendException(Throwable t) {
		AnimoException ae;
		if (t instanceof AnimoException) {
			ae = (AnimoException) t;
			ae.addToStack(op);
		} else {
			ae = new AnimoException(op, t);
		}
		parent.stop.publish(ae);
		done();
	}

	public void done() {
		if (parent == null) answer.publish(null);
		else parent.answer.publish(null);
	}

	protected CountDownLatch waitBeforeClosePipe = null;
	
	public void waitBeforeClosePipe(int count) {
		waitBeforeClosePipe = new CountDownLatch(count);
//		if (parent == null) answer.publish(null);
//		else parent.answer.publish(null);
	}

	public void countDown() {
		waitBeforeClosePipe.countDown();
	}
	
	public void await() {
		if (waitBeforeClosePipe == null) return;
		
		try {
			waitBeforeClosePipe.await();
		} catch (InterruptedException e) {
			sendException(e);
			e.printStackTrace();
		}
	}

	public Manipulator getManipulator() {
		return m;
	}
	
	public List<Relationship> getPFlowPath() {
		return path;
	}

	public Path getFlowPath() {
//		System.out.println("Path:");
//		for (Relationship r : path) {
//			System.out.println(r);
//		}
//		int i = 0;
//		for (Path path : td_flow.traverse(getOPNode())) {
//			System.out.println(" path = "+path);
//			i++;
//		}
//		System.out.println("PFLOW ********************* "+i);
		
		System.out.println("OPNode = "+getOPNode());

		Path first = null;
		for (Path path : td_flow.traverse(getOPNode())) {
			if (first == null) {
				first = path;
			}
			boolean haveUse = false;
			for (Relationship r : path.relationships()) {
				if (r.getType().name().equals(USE._.rType)) {
					haveUse = true;
					break;
				}
			}
			if (!haveUse)
				return path;
		}
		
		return first;

//		Iterator<Path> it = td_flow.traverse(getOPNode()).iterator();
//		if (it.hasNext())
//			return it.next();
//		else {
//			//what looks wrong!
//			return null;
//		}
	}
	
	public Iterable<Relationship> stack() {
		return new PFlowStack();
	}
	
	private class PFlowStack implements Iterator<Relationship>, Iterable<Relationship> {
		
		private Iterator<Relationship> it = getFlowPath().relationships().iterator();
		private Relationship pos = step();
		
		private Relationship step() {
			while (it.hasNext()) {
				Relationship r = it.next();
				if (AN._.relationshipType().name().equals(r.getType().name())){
					return r;
				}
			}
			return null;
		}
		
		@Override
		public boolean hasNext() {
			return pos != null;
		}

		@Override
		public Relationship next() {
			Relationship res = pos;
			pos = step();
			return res;
		}

		@Override
		public void remove() {
			// TODO Auto-generated method stub
		}

		@Override
		public Iterator<Relationship> iterator() {
			return this;
		}
		
	}

	public Iterable<Relationship> getStackContext(Relationship r, boolean goDown) {
//		return td_context.traverse(node).relationships();
		
		Node node = r.getEndNode();
		
		if (goDown && r.getType().name().equals(AN._.relationshipType().name())) {
			node = node.getSingleRelationship(REF, OUTGOING).getEndNode();
		}
		
		return node.getRelationships(AN._.relationshipType(), OUTGOING);
	}


	public void addContextPoint(Relationship r) {
		path.insertElementAt(r, 1);
	}

	public Relationship getLastContext() {
		for (Relationship r : path) {
			if (AN._.rType.equals(r.getType().name()))
				return r; 
		}
		return path.lastElement();
	}

	private TraversalDescription td_flow =
			Traversal.description().depthFirst().
			uniqueness(Uniqueness.RELATIONSHIP_PATH).
			evaluator(new Evaluator(){
				@Override
				public Evaluation evaluate(Path path) {
					//System.out.println(" "+path);
					if (path.length() > 0) {
						Relationship r = path.lastRelationship();
						if (r.getStartNode().equals(path.endNode())) {
							if (r.equals(getStartOP())) {
								return INCLUDE_AND_PRUNE;
							} 
							return EXCLUDE_AND_CONTINUE;	

						//Allow ...<-IS->...
						} if (path.length() > 1 && 
								r.getType().name().equals(IS._.rType)) {
							return EXCLUDE_AND_CONTINUE;
						}
						return EXCLUDE_AND_PRUNE;
					}
					return EXCLUDE_AND_CONTINUE;
				}
			});

	public boolean isInStack(Relationship r) {
		if (op != null && op.equals(r)) return true;
		
		if (parent == null) return false;
		return parent.isInStack(r);
	}
	
	public void debug() {
		StringBuilder sb = new StringBuilder();
		sb.append("DEBUG PFlow ");
		sb.append(PFlowToString(this));
		sb.append("\nPath = ");
		sb.append(Arrays.toString(path.toArray()));
		sb.append("\n");
		sb.append("OPs ");
		ops(sb);
		System.out.println(sb.toString());
	}
	
	private void ops(StringBuilder sb) {
		if (op != null) { sb.append(op); sb.append(" ");}
		if (parent != null) parent.ops(sb);
	}
}
