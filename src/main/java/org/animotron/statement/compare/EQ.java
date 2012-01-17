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
import org.animotron.graph.index.Order;
import org.animotron.io.Pipe;
import org.animotron.manipulator.Evaluator;
import org.animotron.manipulator.OnContext;
import org.animotron.manipulator.PFlow;
import org.animotron.manipulator.QCAVector;
import org.animotron.statement.operator.Operator;
import org.animotron.statement.operator.Predicate;
import org.animotron.statement.operator.REF;
import org.animotron.statement.operator.Utils;
import org.animotron.statement.query.GET;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.index.IndexHits;

import java.io.IOException;
import java.util.List;
import java.util.Set;

/**
 * Compare operator 'EQ'.
 * 
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 */
public class EQ extends Operator implements Predicate {
	
	public static final EQ _ = new EQ();
	
	private EQ() { super("eq"); }

	private static boolean debug = false;
	
	@Override
	public boolean filter(PFlow pf, Relationship op, Relationship ref) throws InterruptedException, IOException {
		if (debug) System.out.println("EQ op "+op+" ref "+ref);

		final QCAVector vector = pf.getVector().answered(ref).question(op);

		//XXX: fix
		final Set<Node> thes = new FastSet<Node>();
		Pipe p = Utils.getByREF(pf, vector);
		QCAVector v;
		while ((v = p.take()) != null) {
			thes.add(v.getAnswer().getEndNode());
		}

		final PFlow pflow = new PFlow(vector);
		
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
				GET._.get(pflow, vector, thes, null);
				pflow.done();
			}
		});
		
		//if (!pipe.hasNext()) return false;
		
		List<QCAVector> actual = new FastList<QCAVector>();

		if (debug) System.out.println("Eval actual");
		QCAVector have;
		while ((have = pipe.take()) != null) { 
			IndexHits<Relationship> hits = Order.context(have.getAnswer().getEndNode());
			try {
				for (Relationship r : hits) {
					Pipe in = Evaluator._.execute(vector.question(r));
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

		List<QCAVector> expected = new FastList<QCAVector>();

		if (debug) System.out.println("Eval expected");
		IndexHits<Relationship> hits = Order.queryDown(op.getEndNode());
		try {
			boolean first = true;
			for (Relationship r : hits) {
				if (first) {
					first = false;
					continue;
				}
				if (r.isType(REF._)) continue;
				
				Pipe in = Evaluator._.execute(vector.question(r));
				QCAVector e;
				while ((e = in.take()) != null) {
					expected.add(e);
					if (debug) System.out.println("expected "+e);
				}
			}
		} finally {
			hits.close();
		}
		
		if (actual.size() == 1 && expected.size() == 1) {
			QCAVector e = actual.get(0);
			QCAVector g = expected.get(0);
			
			if (e.getAnswer().isType(g.getAnswer().getType())
				&& e.getAnswer().getEndNode().equals(g.getAnswer().getEndNode()))
				
				return true;
		}

		return false;
	}
}