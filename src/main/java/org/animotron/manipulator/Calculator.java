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
import static org.animotron.graph.AnimoGraph.getCACHE;
import static org.animotron.graph.AnimoGraph.getROOT;
import static org.animotron.graph.AnimoGraph.getTOP;

import java.io.IOException;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.concurrent.Executor;
import java.util.concurrent.Executors;

import javolution.util.FastList;

import org.animotron.graph.RelationshipTypes;
import org.animotron.io.PipedInputObjectStream;
import org.animotron.io.PipedOutputObjectStream;
import org.animotron.operator.THE;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Path;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.Transaction;
import org.neo4j.graphdb.traversal.TraversalDescription;
import org.neo4j.helpers.Predicate;
import org.neo4j.kernel.Traversal;
import org.neo4j.kernel.Uniqueness;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class Calculator extends GraphListener {
	
	public static Calculator _ = new Calculator();
	
	private Calculator() {
		super(RelationshipTypes.CALC, Creative._);
	}

	private static int THREADS_NUMBER = 100;
	
	private static Executor exec = Executors.newFixedThreadPool(THREADS_NUMBER);
	
	public PipedInputObjectStream eval(Relationship op) throws IOException {
		PipedInputObjectStream in = new PipedInputObjectStream();
		exec.execute(Evaluator._.walk(op, new PipedOutputObjectStream(in)));
		return in;
	}

	public PipedInputObjectStream eval(Node op) throws IOException {
		PipedInputObjectStream in = new PipedInputObjectStream();
		exec.execute(Evaluator._.walk(op, new PipedOutputObjectStream(in)));
		return in;
	}

	public static void eval(Relationship op, PipedOutputObjectStream out) {
		exec.execute(Evaluator._.walk(op, out));
	}
	
	public List<Relationship> evalGetResult(Relationship op) throws IOException {
		PipedInputObjectStream in = new PipedInputObjectStream();
		exec.execute(Evaluator._.walk(op, new PipedOutputObjectStream(in)));
		
		List<Relationship> result = new FastList<Relationship>();
		for (Object obj : in) {
			if (obj instanceof Relationship) {
				result.add((Relationship) obj);
			} else
				System.out.println("evalGetResult");
		}
		return result;
	}
	
	public List<Relationship> evalGetResult(Node op) throws IOException {
		PipedInputObjectStream in = new PipedInputObjectStream();
		exec.execute(Evaluator._.walk(op, new PipedOutputObjectStream(in)));
		
		List<Relationship> result = new FastList<Relationship>();
		for (Object obj : in) {
			if (obj instanceof Relationship) {
				result.add((Relationship) obj);
			} else
				System.out.println("evalGetResult");
		}
		return result;
	}

	public PipedInputObjectStream prepare(Relationship op) throws IOException {
		PipedInputObjectStream in = new PipedInputObjectStream();
		exec.execute(Preparator._.walk(op, new PipedOutputObjectStream(in)));
		return in;
	}

	public void prepare(Relationship op, PipedOutputObjectStream out) {
		exec.execute(Preparator._.walk(op, out));
	}
	
	public PipedInputObjectStream filter(Relationship op) throws IOException {
		PipedInputObjectStream in = new PipedInputObjectStream();
		exec.execute(Filter._.walk(op, new PipedOutputObjectStream(in)));
		return in;
	}

	public void filter(Relationship op, PipedOutputObjectStream out) {
		exec.execute(Filter._.walk(op, out));
	}
	
    private class Rule implements Predicate<Path> {

		private final HashSet<Node> set = new HashSet<Node>();
		{
			set.add(getROOT());
			set.add(getTOP());
			set.add(getCACHE());
			set.add(getRoot());
		}
		
		@Override
		public boolean accept(Path pos) {
			if(pos.endNode().equals(THE._.NODE())) {
	        	for (Node i : pos.nodes()) {
	        		if (set.contains(i))
	        			return false;
	        	}
	        	return true;
			} else {
				return false;
			}
		}
    	
    }
    
    @SuppressWarnings("deprecation")
    private final TraversalDescription TD = Traversal.description()
    	.depthFirst().uniqueness(Uniqueness.RELATIONSHIP_PATH).filter(new Rule());

	@Override
    public void push(final Relationship op, PipedOutputObjectStream out) {
		
		if (true) return;
		
		System.out.println("Prepare the relationship " + op);
		
        HashSet<Relationship> set = new HashSet<Relationship>();
		
		Transaction tx = beginTx();
		try {
			
            Iterator<Path> pi = TD.traverse(op.getEndNode()).iterator();
            
			//System.out.println("Found paths:");
			while (pi.hasNext()) {
				Path path = pi.next();
				//System.out.println("	" + path);
				set.add(path.lastRelationship());
			}
			
			tx.success();

		} finally {
			finishTx(tx);
		}
			
		try {
			//System.out.println("Found relationships:");
			for (Relationship r : set) {
				//System.out.println("	" + r);
				prepare(r);
			}
		} catch (IOException e) {
			e.printStackTrace();
		}
		
	}

}
