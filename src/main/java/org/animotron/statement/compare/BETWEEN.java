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
import org.animotron.manipulator.Evaluator;
import org.animotron.manipulator.OnContext;
import org.animotron.manipulator.PFlow;
import org.animotron.manipulator.QCAVector;
import org.animotron.statement.operator.*;
import org.animotron.statement.query.GET;
import org.animotron.statement.string.STRING;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;

import java.io.IOException;
import java.util.List;
import java.util.Set;

/**
 * Compare operator 'BETWEEN'.
 * 
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 */
public class BETWEEN extends Operator implements Predicate {
	
	public static final BETWEEN _ = new BETWEEN();
	
	private BETWEEN() { super("between"); }
	
	private static boolean debug = false;

	@Override
	public boolean filter(final PFlow pf, Relationship op, final Relationship ref) throws InterruptedException, IOException {
		
		if (debug) 
			System.out.println("BETWEEN op "+op+" ref "+ref);
		
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

		//XXX: fix
		if (actual.size() >= 1 && expected.size() == 2) {
			String first = STRING._.eval(pf, expected.get(0).getClosest()).toString();
			String second = STRING._.eval(pf, expected.get(1).getClosest()).toString();

			if (debug) System.out.println("***** expected ["+first+" "+second+"]");
			
			for (QCAVector e : actual) {
				if (debug) System.out.println("***** actual = "+e.getAnswer().getEndNode());
				String answer = STRING._.eval(pf, e.getClosest()).toString();
				if (answer.compareTo(first) >= 0 && answer.compareTo(second) <= 0)
					return true;
			}
		}

		return false;
	}

	public Set<Relationship> getExpected(final PFlow pf, final Relationship op) throws InterruptedException, IOException {
		return null;
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
}
