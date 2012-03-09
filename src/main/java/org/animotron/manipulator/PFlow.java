/*
 *  Copyright (C) 2011-2012 The Animo Project
 *  http://animotron.org
 *  	
 *  This file is part of Animotron.
 *  
 *  Animotron is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU Affero General Public License as 
 *  published by the Free Software Foundation, either version 3 of 
 *  the License, or (at your option) any later version.
 *  
 *  Animotron is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Affero General Public License for more details.
 *  
 *  You should have received a copy of 
 *  the GNU Affero General Public License along with Animotron.  
 *  If not, see <http://www.gnu.org/licenses/>.
 */
package org.animotron.manipulator;

import javolution.util.FastMap;
import org.animotron.exception.AnimoException;
import org.animotron.io.Pipe;
import org.animotron.statement.operator.Utils;
import org.animotron.utils.MessageDigester;
import org.jetlang.channels.Channel;
import org.jetlang.channels.MemoryChannel;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.RelationshipType;

import java.io.ByteArrayOutputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.security.MessageDigest;
import java.util.Map;
import java.util.concurrent.CountDownLatch;

import static org.animotron.graph.RelationshipTypes.RESULT;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class PFlow {
	
	private Channel<QCAVector> aChannel = null;
	private Channel<PFlow> qChannel = null;
	private Channel<Throwable> sChannel = null;
	
	private Controller controller;
	private QCAVector path;
	
	public PFlow(Controller controller, Relationship op) {
		path = new QCAVector(op);
		
		this.controller = controller;
	}

	public PFlow(Controller controller, QCAVector vector) {
		path = vector;

		this.controller = controller;
	}
	
	public Controller getController() {
		return controller;
	}

	public Channel<PFlow> questionChannel() {
		if (qChannel == null)
			qChannel = new MemoryChannel<PFlow>();
		
		return qChannel;
	}

	public Channel<QCAVector> answerChannel() {
		if (aChannel == null)
			aChannel = new MemoryChannel<QCAVector>();
		
		return aChannel;
	}

	public Channel<Throwable> stopChannel() {
		if (sChannel == null)
			sChannel = new MemoryChannel<Throwable>();
		
		return sChannel;
	}

	protected void cyclingDetection() throws AnimoException {
		cyclingDetection(getOP());
	}

	private void cyclingDetection(Relationship op) throws AnimoException {
		int deep = 0; int count = 0;
		//for (QCAVector v : path) {
			if (deep > 0 && path.hasRelationship(op)) {
				if (count > 2)
					throw new AnimoException(op, "cycling detected "+path);
                else
					count++;
			}
			deep++;
		//}
	}

	public Relationship getOP() {
		return path.getClosest();
	}

	public Node getOPNode() {
		return path.getClosest().getEndNode();
	}

	public void sendAnswer(QCAVector r) {
		//System.out.println(getOP()+" answer "+r);
		answerChannel().publish(r);
	}

	public void sendAnswer(Relationship answer) {
		//if (getOP().getId() == 7786) {
		//	System.out.print("answered ");
		//	Utils.debug(answer);
		//}
		sendAnswer(answer, RESULT, getPathHash());
	}

	public void sendAnswer(Relationship answer, RelationshipType rType, byte[] hash) {
		Relationship createdAnswer = Utils.createResult( this, getOPNode(), answer, rType, hash );
		
		answerChannel().publish(path.answered(createdAnswer));
	}

	public void sendAnswer(QCAVector answerVector, RelationshipType rType) {
		Relationship answer = Utils.createResult(this, answerVector.getContext(), getOPNode(), answerVector.getAnswer(), rType);
		
		answerChannel().publish(new QCAVector(getOP(), answer, answerVector.getContext(), answerVector.getPrecedingSibling()));
	}

	public void sendAnswer(Relationship answer, QCAVector context) {
		Relationship createdAnswer = Utils.createResult(this, getOPNode(), answer, RESULT);

		sendAnswer(getOP(), context, createdAnswer);
	}


	public void sendAnswer(Relationship question, QCAVector context, Relationship answer) {
		answerChannel().publish(new QCAVector(question, context, answer));
	}

	public void sendException(Throwable t) {
		//t.printStackTrace();
		
		AnimoException ae;
		if (t instanceof AnimoException) {
			ae = (AnimoException) t;
			ae.addToStack(getOP());
		} else {
			ae = new AnimoException(getOP(), t);
		}
		stopChannel().publish(ae);
		//done();
	}

	public void done() {
		//await();
		answerChannel().publish(null);
		
		//System.out.println("Done "+getVector());
		
		//dispose();
	}

	protected CountDownLatch waitBeforeClosePipe = null;
	
	public CountDownLatch waitBeforeClosePipe(int count) {
		waitBeforeClosePipe = new CountDownLatch(count);
		
		return waitBeforeClosePipe;
	}

	public void countDown() {
		waitBeforeClosePipe.countDown();
		//System.out.println("countDown "+waitBeforeClosePipe.getCount()+" "+this);
	}
	
	public void countDown(Pipe pipe) {
		if (waitBeforeClosePipe == null)
			pipe.close();

		waitBeforeClosePipe.countDown();
		if (waitBeforeClosePipe.getCount() == 0)
			pipe.close();
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

//	public Iterable<Relationship> getStackContext(Relationship r, boolean goDown) {
////		return td_context.traverse(node).relationships();
//		
//		Node node = r.getEndNode();
//		
//		if (goDown && r.isType(AN._)) {
//			node = node.getSingleRelationship(REF, OUTGOING).getEndNode();
//		}
//		
//		return node.getRelationships(AN._, OUTGOING);
//	}

	public byte[] getPathHash() {
		ByteArrayOutputStream bos = new ByteArrayOutputStream();  
		DataOutputStream dos = new DataOutputStream(bos);  
		
		if (path == null) return new byte[0];
		try {
			path.collectHash(dos);
		} catch (IOException e) {
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
		if (debug) System.out.println(path);
		return path.getQuestion();
	}

	public boolean isInStack(Relationship r) {
		boolean debug = false;
		if (debug) System.out.println("IN STACK CHECK "+r+" in "+path+" ");
		//for (QCAVector v : path) {
			if (path.hasRelationship(r)) {
				if (debug) System.out.println("FOUND!!!");
				return true;
			}
		//}
		if (debug) System.out.println("NOT FOUND");
		return false;

	}
	
	public boolean getFromStack(Relationship r) {
		boolean debug = false;
		if (debug) System.out.println("IN STACK CHECK "+r+" in "+path+" ");
		//for (QCAVector v : path) {
			if (path.hasRelationship(r)) {
				if (debug) System.out.println("FOUND!!!");
				return true;
			}
		//}
		if (debug) System.out.println("NOT FOUND");
		return false;

	}

	public void debug() {
		StringBuilder sb = new StringBuilder();
		sb.append("DEBUG PFlow ");
		//sb.append(Utils.shortID(this));
		sb.append("\nPath = ");
		sb.append(path);
		sb.append("\n");
		sb.append("OPs ");
		ops(sb);
		System.out.println(sb.toString());
	}
	
	private void ops(StringBuilder sb) {
		if (getOP() != null) { sb.append(getOP()); sb.append(" ");}
	}

	public QCAVector getVector() {
		return path;
	}
	
	private Map<Object, Object> data = null;
	
	private Map<Object, Object> getData() {
		if (data == null) {
			data = new FastMap<Object, Object>();
		}
		return data;
	}

	public boolean haveData(Object key) {
		return getData().containsKey(key);
	}

	public Object getData(Object key) {
		return getData().get(key);
	}

	public void putData(Object key, Object value) {
		getData().put(key, value);
	}
	
	//FastTable<Fiber> queues = new FastTable<Fiber>(); 

//	public void subscribe(Channel channel, Subscribable callback) {
//		channel.subscribe(callback.getQueue(), callback);
//        
//        //queues.add(callback.getFiber());
//	}
	
//	private void dispose() {
//		for (int i = 0, n = queues.size(); i < n; i++) {
//			queues.get(i).dispose();
//		}
//	}
}
