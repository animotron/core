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
package org.animotron.statement.compare;

import javolution.util.FastList;
import javolution.util.FastSet;

import org.animotron.Executor;
import org.animotron.graph.index.Order;
import org.animotron.io.PipedInput;
import org.animotron.io.PipedOutput;
import org.animotron.manipulator.Evaluator;
import org.animotron.manipulator.PFlow;
import org.animotron.manipulator.QCAVector;
import org.animotron.statement.Statement;
import org.animotron.statement.Statements;
import org.animotron.statement.operator.*;
import org.animotron.statement.query.GET;
import org.jetlang.channels.Subscribable;
import org.jetlang.core.DisposingExecutor;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.index.IndexHits;

import java.io.IOException;
import java.util.List;
import java.util.Set;

/**
 * Compare operator 'WITH'.
 * 
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 */
public class WITH extends Operator implements Predicate {
	
	public static final WITH _ = new WITH();
	
	private WITH() { super("with"); }

	@Override
	public boolean filter(PFlow pf, Relationship op, Relationship ref) throws InterruptedException, IOException {
		
		//System.out.println("==================================================");
		//System.out.println("WITH op "+op+" ref "+ref);
		//XXX: fix
		Node theNode = Utils.getSingleREF(op.getEndNode());
		
		Set<Node> thes = new FastSet<Node>();
		thes.add(theNode);

		final PipedOutput<QCAVector> out = new PipedOutput<QCAVector>();
		PipedInput<QCAVector> in = out.getInputStream();
		
		QCAVector vector = pf.getVector().answered(ref);
		
		PFlow pflow = new PFlow(new PFlow(Evaluator._), vector.question(op));
		
		pflow.getParent().answerChannel().subscribe(new Subscribable<QCAVector>() {
			
			@Override
			public void onMessage(QCAVector message) {
				try {
					if (message == null)
						out.close();
					
					out.write(message);
				} catch (IOException e) {
				}
			}
			
			@Override
			public DisposingExecutor getQueue() {
				return Executor.getFiber();
			}
		});

		GET._.get(pflow, op, vector, thes, null);
		pflow.done();
		
		//if (!in.hasNext()) return false;
		
		List<QCAVector> actual = new FastList<QCAVector>();
		List<QCAVector> expected = new FastList<QCAVector>();

		System.out.println("Eval actual");
		for (QCAVector have : in) {
			System.out.println("actual get "+have);
			IndexHits<Relationship> hits = Order.queryDown(have.getAnswer().getEndNode());
			try {
				for (Relationship r : hits) {
					in = Evaluator._.execute(new PFlow(pf), have.question(r));
					for (QCAVector e : in) {
						actual.add(e);
						System.out.println("actual "+e);
					}
				}
			} finally {
				hits.close();
			}
		}
		
		if (actual.isEmpty()) return false;

		System.out.println("Eval expected");
		in = Evaluator._.execute(new PFlow(pf), op.getEndNode());
		for (QCAVector e : in) {
			expected.add(e);
			System.out.println("expected "+e);
		}
		
		if (actual.size() >= 1 && expected.size() == 1) {
			QCAVector g = expected.get(0);
			
			//XXX: finish
			List<QCAVector> l = evaluable(pf, g.getAnswer().getEndNode());
			if (l.size() == 1)
				g = l.get(0);
			else if (l.size() > 1) {
				
				System.out.println("DON'T KNOW WHAT TO DO, get alot of results @WITH");
//				for (ACQVector r : l) {
//					System.out.println(""+r+" "+r.getType());
//				}
			}

			System.out.println("***** expected = "+g.getAnswer().getEndNode());
			
			for (QCAVector e : actual) {
				System.out.println("***** actual = "+e.getAnswer().getEndNode());
				if (e.getAnswer().isType(g.getAnswer().getType())
					&& e.getAnswer().getEndNode().equals(g.getAnswer().getEndNode()))
					
					return true;
			}
		}

		return false;
	}
	
	private List<QCAVector> evaluable(PFlow pf, Node node) throws InterruptedException, IOException {
		List<QCAVector> list = new FastList<QCAVector>();
		
		IndexHits<Relationship> q = Order.queryDown(node);
		try {
			for (Relationship i : q) {
				if (i.isType(REF._)) continue;
				
				Statement s = Statements.relationshipType(i);
    			if (s instanceof Query || s instanceof Evaluable) {
    				//System.out.println("+++++++++++++++++++++++++++++++++++++++++ get evaluable");
    				PipedInput<QCAVector> in = Evaluator._.execute(pf, i);
    				for (QCAVector e : in) {
    					list.add(e);
    					//System.out.println("get from Evaluator "+r);
    				}
    			} else {
    				list.add(new QCAVector(null, i));
    			}
			}
		} finally {
			q.close();
		}

		
		return list;
	}
}
