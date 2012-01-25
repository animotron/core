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
package org.animotron.statement.compare;

import static org.neo4j.graphdb.Direction.INCOMING;
import static org.neo4j.graphdb.Direction.OUTGOING;
import javolution.util.FastList;
import javolution.util.FastSet;

import org.animotron.Executor;
import org.animotron.graph.index.Order;
import org.animotron.io.Pipe;
import org.animotron.manipulator.Controller;
import org.animotron.manipulator.Evaluator;
import org.animotron.manipulator.OnContext;
import org.animotron.manipulator.PFlow;
import org.animotron.manipulator.QCAVector;
import org.animotron.statement.Statement;
import org.animotron.statement.Statements;
import org.animotron.statement.operator.*;
import org.animotron.statement.query.GET;
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
	
	private static boolean debug = false;

	@Override
	public boolean filter(final PFlow pf, Relationship op, final Relationship ref) throws InterruptedException, IOException {
		
		if (debug) 
			System.out.println("WITH op "+op+" ref "+ref);
		//XXX: fix
		
		QCAVector qVector = null;

		List<QCAVector> expected = getExpecting(pf, op);

		qVector = pf.getVector().question2(op);

		final Set<Node> thes = new FastSet<Node>();
		Utils.getTHEbag(pf, qVector, thes);

		final PFlow pflow = new PFlow(pf.getController(), qVector);
		
		final Pipe pipe = Pipe.newInstance();
		
		OnContext onContext = new OnContext() {
			@Override
			public void onMessage(QCAVector vector) {
				super.onMessage(vector, pipe);
			}
		};
		onContext.setCountDown(1);
		pflow.answerChannel().subscribe(onContext);

		Executor.execute(new Runnable() {
			@Override
			public void run() {
				QCAVector aVector = pf.getVector().answered(ref);
				GET._.get(pflow, aVector, thes, null);
				pflow.done();
			}
		});
		
		//if (!in.hasNext()) return false;
		
		List<QCAVector> actual = new FastList<QCAVector>();

		if (debug) 
			System.out.println("Eval actual");
		
		QCAVector have;
		while ((have = pipe.take()) != null) {
			if (debug) System.out.println("actual get "+have);
			IndexHits<Relationship> hits = Order.context(have.getClosest().getEndNode());
			try {
				for (Relationship r : hits) {
					Pipe in = Evaluator._.execute(pf.getController(), have.question(r));
					QCAVector e;
					while ((e = in.take()) != null) {
						actual.add(e);
						if (debug) System.out.println("actual "+e);
					}
				}
			} finally {
				hits.close();
			}
		}
		
		if (actual.isEmpty()) return false;

		if (actual.size() >= 1 && expected.size() == 1) {
			QCAVector g = expected.get(0);
			
			//XXX: finish
			List<QCAVector> l = evaluable(pf.getController(), g);
			if (l.size() == 1)
				g = l.get(0);
			else if (l.size() > 1) {
				
				System.out.println("DON'T KNOW WHAT TO DO, get alot of results @WITH");
				for (QCAVector r : l) {
					System.out.println(""+r+" "+r.getClosest().getType());
				}
				System.out.println("expected "+g);
			}

			if (debug) System.out.println("***** expected = "+g.getAnswer().getEndNode());
			
			for (QCAVector e : actual) {
				if (debug) System.out.println("***** actual = "+e.getAnswer().getEndNode());
				if (e.getAnswer().isType(g.getAnswer().getType())
					&& e.getAnswer().getEndNode().equals(g.getAnswer().getEndNode()))
					
					return true;
			}
		}

		return false;
	}

	public Set<Relationship> getExpected(final PFlow pf, final Relationship op) throws InterruptedException, IOException {
		
		Set<Relationship> thes = new FastSet<Relationship>();
		
		final long ref = op.getEndNode().getSingleRelationship(REF._, OUTGOING).getEndNode().getId();
		
		QCAVector qVector = pf.getVector().answered(pf.getVector().getClosest());
		Pipe in = Evaluator._.execute(pf.getController(), qVector, op.getEndNode());
		QCAVector e;
		while ((e = in.take()) != null) {
			
			final Relationship r = e.getClosest();

			for (Relationship rr : r.getEndNode().getRelationships(r.getType(), INCOMING)) {
				
				if (rr.getStartNode().getSingleRelationship(REF._, OUTGOING).getEndNode().getId() == ref) {
					
					for (Relationship rrr : rr.getStartNode().getRelationships(AN._, INCOMING)) {
						
						for (Relationship the : rrr.getStartNode().getRelationships(THE._, INCOMING)) {
							thes.add( the );
						}
					}
				}
			}
		}
		
		return thes;
	}
	
	
	public List<QCAVector> getExpecting(final PFlow pf, final Relationship op) throws InterruptedException, IOException {
		
		List<QCAVector> expected = (List<QCAVector>) pf.getData(op);

		if (expected == null) {
			if (debug) 
				System.out.println("Eval expected");
			
			expected = new FastList<QCAVector>();
			
			QCAVector qVector = pf.getVector().answered(pf.getVector().getClosest());
			Pipe in = Evaluator._.execute(pf.getController(), qVector, op.getEndNode());
			QCAVector e;
			while ((e = in.take()) != null) {
				expected.add(e);
				if (debug) System.out.println("expected "+e);
			}
			pf.putData(op, expected);
		}
		
		return expected;
	}

	private List<QCAVector> evaluable(final Controller controller, final QCAVector vector) throws InterruptedException, IOException {
		List<QCAVector> list = new FastList<QCAVector>();
		
		IndexHits<Relationship> q = Order.context(vector.getClosest().getEndNode());
		try {
			for (Relationship i : q) {
				Statement s = Statements.relationshipType(i);
    			if (s instanceof Query || s instanceof Evaluable) {
    				//System.out.println("+++++++++++++++++++++++++++++++++++++++++ get evaluable");
    				Pipe in = Evaluator._.execute(controller, vector.question(i));
    				QCAVector e;
    				while ((e = in.take()) != null) {
    					list.add(e);
    					System.out.println("get from Evaluator "+e);
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
