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

import javolution.util.FastList;
import javolution.util.FastSet;
import org.animotron.Executor;
import org.animotron.io.Pipe;
import org.animotron.manipulator.*;
import org.animotron.statement.Statement;
import org.animotron.statement.Statements;
import org.animotron.statement.operator.*;
import org.animotron.statement.query.GET;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;

import java.io.IOException;
import java.util.List;
import java.util.Set;

import static org.animotron.graph.Properties.FREEZE;
import static org.neo4j.graphdb.Direction.INCOMING;
import static org.neo4j.graphdb.Direction.OUTGOING;

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
				try {
					GET._.get(pflow, aVector, thes, null);
				} finally {
					pflow.done();
				}
			}
		});
		
		//if (!in.hasNext()) return false;
		
		List<QCAVector> actual = new FastList<QCAVector>();

		if (debug) 
			System.out.println("Eval actual");
		
		QCAVector have;
		while ((have = pipe.take()) != null) {
			if (debug) System.out.println("actual get "+have);

			Relationship h = have.getClosest();
			if (h.isType(AN._)) {
				Pipe in = AN._.getREFs(pf, have);
				QCAVector e;
				while ((e = in.take()) != null) {
					actual.add(e);
					if (debug) System.out.println("actual "+e);
				}
			} else {
				actual.add(have);
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
		
		
		long ref = -1; 
		for (Relationship r : op.getEndNode().getRelationships(REF._, OUTGOING))
			if (!FREEZE.has(r)) {
				ref = r.getEndNode().getId();
				break;
			}
		if (ref == -1) return null;
		
		//System.out.println(ref);
		
		QCAVector qVector = pf.getVector();//.answered(pf.getVector().getClosest());
		Pipe in = Evaluator._.execute(pf.getController(), qVector, op.getEndNode());
		QCAVector v;
		while ((v = in.take()) != null) {
			
			final Relationship r = v.getClosest();
			
			//System.out.println("==============");
			//System.out.println(r);

			for (Relationship rr : r.getEndNode().getRelationships(r.getType(), INCOMING)) {

				//System.out.println(" "+rr);
				
				try {
					Relationship rf = rr.getStartNode().getSingleRelationship(REF._, OUTGOING);
					
					//System.out.println("  "+rf);

					if (rf != null && rf.getEndNode().getId() == ref) {
						
						for (Relationship rrr : rr.getStartNode().getRelationships(AN._, INCOMING)) {
							
							//System.out.println("   "+rrr);
							
							for (Relationship the : rrr.getStartNode().getRelationships(DEF._, INCOMING)) {
								thes.add( the );
							}
						}
					}
				} catch (Throwable t) {};
			}
		}
		
		return thes;
	}
	
	
	public List<QCAVector> getExpecting(final PFlow pf, final Relationship op) throws InterruptedException, IOException {
		
		@SuppressWarnings("unchecked")
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
		
		if (vector.getClosest().isType(REF._)) {
			list.add(vector);
			return list;
		}
		
//		IndexHits<Relationship> q = Order._.context(vector.getClosest().getEndNode());
//		try {
//			for (Relationship i : q) {
		Relationship i = vector.getClosest();
		Statement s = Statements.relationshipType(i);
		if (s instanceof Query || s instanceof Evaluable) {
			//System.out.println("+++++++++++++++++++++++++++++++++++++++++ getDef evaluable");
			Pipe in = Evaluator._.execute(controller, vector.question(i));
			QCAVector e;
			while ((e = in.take()) != null) {
				list.add(e);
				System.out.println("get from Evaluator "+e);
			}
		} else {
			list.add(new QCAVector(null, i));
		}
//			}
//		} finally {
//			q.close();
//		}
		
		return list;
	}
	
}
