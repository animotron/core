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
import org.animotron.io.PipedOutput;
import org.animotron.statement.Statement;
import org.animotron.statement.Statements;
import org.animotron.statement.operator.AN;
import org.animotron.statement.operator.Reference;
import org.animotron.statement.operator.Utils;
import org.animotron.statement.relation.USE;
import org.animotron.utils.MessageDigester;
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

import java.io.ByteArrayOutputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.security.MessageDigest;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.Vector;
import java.util.concurrent.CountDownLatch;

import static org.animotron.graph.RelationshipTypes.REF;
import static org.animotron.graph.RelationshipTypes.RESULT;
import static org.neo4j.graphdb.Direction.OUTGOING;
import static org.neo4j.graphdb.traversal.Evaluation.*;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class PFlow {
	
	private final Manipulator m;

	public final Channel<QCAVector> answer = new MemoryChannel<QCAVector>();
	public final Channel<PFlow> question = new MemoryChannel<PFlow>();
	public final Channel<Throwable> stop = new MemoryChannel<Throwable>();
	
	protected final PFlow parent;
	private Relationship op = null;
	private Node opNode = null;
	
	private Vector<QCAVector> path = new Vector<QCAVector>();
	
	public PFlow(Manipulator m) {
		parent = null;
		this.m = m;

	}

	public PFlow(Manipulator m, Relationship op) {
		parent = null;
		this.m = m;

		this.op = op; 
		path.add(new QCAVector(op));
	}

	public PFlow(PFlow parent) {
		this.parent = parent;
		this.m = parent.m;
		
		//XXX: maybe, clone faster?
		path.addAll(parent.path);
	}

	public PFlow(PFlow parent, Relationship op) throws AnimoException {
//		System.out.print("new PFlow ");
//		System.out.println("this = "+Utils.shortID(this)+" parent = "+Utils.shortID(parent));
//		System.out.print(" "+(new IOException()).getStackTrace()[1]);
//		System.out.println(" "+op);
		
		this.parent = parent;
		this.m = parent.m;
		
		//XXX: maybe, clone faster?
		path.addAll(parent.path);
		
		cyclingDetection(op);

		//XXX: remove?
		Statement s = Statements.relationshipType(op);
		if (s instanceof Reference || op.isType(REF))
			addContextPoint(op);
		
		else if (op.isType(RESULT))
			addContextPoint(new QCAVector(null,Utils.relax(op)));
			
		if (path.isEmpty())
			path.add(new QCAVector(op));
		
		this.op = op;
	}

	public PFlow(PFlow parent, Node opNode) {
		this.parent = parent;
		this.m = parent.m;
		
		//XXX: maybe, clone faster?
		path.addAll(parent.path);

		this.opNode = opNode;
	}
	
	public PFlow(PFlow parent, QCAVector vector) {
		this.parent = parent;
		this.m = parent.m;
		
		//XXX: maybe, clone faster?
		path.addAll(parent.path);
		
		addContextPoint(vector);

		this.op = vector.getUnrelaxedClosest();
	}

	private void cyclingDetection(Relationship op) throws AnimoException {
		int deep = 0; int count = 0;
		for (QCAVector v : path) {
			if (deep > 0 && v.haveRelationship(op)) {
				if (count > 2)
					throw new AnimoException(op, "cycling detected "+path);
				else
					count++;
			}
			deep++;
		}
	}

	public PFlow getParent() {
		return parent;
	}

	public Relationship getOP() {
		return op;
	}

	//XXX: still required?
	public Relationship getStartOP() {
		return path.lastElement().getQuestion();
	}
	
	//XXX: still required?
	public Node getStartNode() {
		return path.lastElement().getQuestion().getEndNode();
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

	public void sendAnswer(QCAVector r) {
		if (parent == null) {
			System.out.println("WORNG - no parent");
			throw new IllegalArgumentException("NULL parent @pflow"); 
		} else {
			parent.answer.publish(r);
		}
	}

	public void sendAnswer(Relationship answer) {
		sendAnswer(answer, RESULT, getPathHash());
	}

	public void sendAnswer(Relationship answer, RelationshipType rType, byte[] hash) {
		if (parent == null) {
			System.out.println("WORNG - no parent");
			throw new IllegalArgumentException("NULL parent @pflow"); 
		} else {
			//System.out.println("send answer to "+parent.answer+" (parent = "+parent+")");

			Relationship createdAnswer = Utils.createResult( this, op.getEndNode(), answer, rType, hash );
			
			parent.answer.publish(new QCAVector(op, createdAnswer));
		}
	}

	public void sendAnswer(QCAVector answerVector, RelationshipType rType) {
		if (parent == null) {
			System.out.println("WORNG - no parent");
			throw new IllegalArgumentException("NULL parent @pflow"); 
		} else {
			//System.out.println("send answer to "+parent.answer+" (parent = "+parent+")");

			Relationship answer = Utils.createResult(this, answerVector.getContext(), op.getEndNode(), answerVector.getAnswer(), AN._);
			
			parent.answer.publish(new QCAVector(op, answer, answerVector.getContext()));
		}
	}

	public void sendAnswer(Relationship answer, QCAVector context) {
		Relationship createdAnswer = Utils.createResult(this, op.getEndNode(), answer, RESULT);

		sendAnswer(op, context, createdAnswer);
	}


	public void sendAnswer(Relationship question, QCAVector context, Relationship answer) {
		if (parent == null) {
			System.out.println("WORNG - no parent");
			throw new IllegalArgumentException("NULL parent @pflow"); 
		} else {
			//System.out.println("send answer to "+parent.answer+" (parent = "+parent+")");

			parent.answer.publish(new QCAVector(question, context, answer));
		}
	}

	public void sendException(Throwable t) {
		t.printStackTrace();
		
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
		//System.out.println("waitBeforeClosePipe "+count+" "+this);
		waitBeforeClosePipe = new CountDownLatch(count);
//		if (parent == null) answer.publish(null);
//		else parent.answer.publish(null);
	}

	public void countDown() {
		if (waitBeforeClosePipe == null)
			waitBeforeClosePipe(1);
		
		waitBeforeClosePipe.countDown();
		//System.out.println("countDown "+waitBeforeClosePipe.getCount()+" "+this);
	}
	
	public void countDown(PipedOutput<?> out) {
		if (waitBeforeClosePipe == null) {
			try {
				out.close();
			} catch (IOException e) {
				e.printStackTrace();
			}
			return;
		}
		
		waitBeforeClosePipe.countDown();
		if (waitBeforeClosePipe.getCount() == 0)
			try {
				out.close();
			} catch (IOException e) {
				e.printStackTrace();
			}
		//System.out.println("countDown "+waitBeforeClosePipe.getCount()+" "+this);
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
	
	public List<QCAVector> getPFlowPath() {
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
		
		//System.out.println("OPNode = "+getOPNode());

		Path first = null;
		for (Path path : get_td_flow().traverse(getOPNode())) {
			if (first == null) {
				first = path;
			}
			boolean haveUse = false;
			for (Relationship r : path.relationships()) {
				if (r.isType(USE._)) {
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
				if (r.isType(AN._)){
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
		
		if (goDown && r.isType(AN._)) {
			node = node.getSingleRelationship(REF, OUTGOING).getEndNode();
		}
		
		return node.getRelationships(AN._, OUTGOING);
	}


	public QCAVector addContextPoint(QCAVector vector) {
		boolean debug = false;
		if (debug) System.out.print("adding "+this+" "+vector);
//		System.out.println(new IOException().getStackTrace()[1]);
		if (path.isEmpty()) {
			if (debug) System.out.println(" (added)");
			path.insertElementAt(vector, 0);
			return vector;
			
		} else if (!path.isEmpty()) {
			QCAVector v = path.get(0);
			if (v.canBeMerged(vector)) {
				path.set(0, vector);
				if (debug) System.out.println(" (merge)");
				return vector;
			} else {
				path.insertElementAt(vector, 0);
				if (debug) System.out.println(" (added)");
				return vector;
			}
		}
		if (debug) System.out.println(" (ignored)");
		return null;
	}

	public QCAVector addContextPoint(Relationship r) {
		return addContextPoint(new QCAVector(r));
	}

	public void popContextPoint(QCAVector vector) {
		//System.out.println("pop "+this+" "+path);
		if (vector == null) return;
		
		if (path.size() == 0) {
			System.out.println("WARNING - path is empty");
			return;
		}
		
		if (path.get(0) == vector)
			path.remove(0);
	}

	public byte[] getPathHash() {
		ByteArrayOutputStream bos = new ByteArrayOutputStream();  
		DataOutputStream dos = new DataOutputStream(bos);  
		
		for (QCAVector p : path) {
			try {
				p.collectHash(dos);
			} catch (IOException e) {
			}
		}
		MessageDigest md = MessageDigester.md();
		md.update(bos.toByteArray());
		return md.digest();
	}

	public byte[] getOpHash() {
		ByteArrayOutputStream bos = new ByteArrayOutputStream();  
		DataOutputStream dos = new DataOutputStream(bos);  
		
		try {
			dos.writeLong(getOP().getId());
		} catch (IOException e) {
		}

		MessageDigest md = MessageDigester.md();
		md.update(bos.toByteArray());
		return md.digest();
	}

	public Relationship getLastContext() {
		boolean debug = false;
		if (debug) System.out.print("PFlow get last context ");
//		for (QCAVector v : path) {
//			Relationship r = v.getQuestion();
//			if (r.isType(AN._)) {
//				if (debug) System.out.println(r);
//				return r;
//			} else if (r.isType(REF)) {
//				if (debug) System.out.println(r);
//				return r;
//			} else if (r.isType(RESULT)) {
//				if (debug) System.out.println(r);
//				return r;
//			}
//		}
		if (debug) System.out.println(path.lastElement());
		return path.lastElement().getQuestion();
	}

	private TraversalDescription get_td_flow() {
		return Traversal.description().depthFirst().
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
								r.isType(AN._)) {
							return EXCLUDE_AND_CONTINUE;
						}
						return EXCLUDE_AND_PRUNE;
					}
					return EXCLUDE_AND_CONTINUE;
				}
			});
	}

	public boolean isInStack(Relationship r) {
		boolean debug = false;
		if (debug) System.out.println("IN STACK CHECK "+r+" in "+path+" ");
		for (QCAVector v : path) {
			if (v.haveRelationship(r)) {
				if (debug) System.out.println("FOUND!!!");
				return true;
			}
		}
		if (debug) System.out.println("NOT FOUND");
		return false;

	}
	
	public void debug() {
		StringBuilder sb = new StringBuilder();
		sb.append("DEBUG PFlow ");
		//sb.append(Utils.shortID(this));
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
