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
import org.animotron.io.PipedInput;
import org.animotron.io.PipedOutput;
import org.animotron.manipulator.Evaluator;
import org.animotron.manipulator.PFlow;
import org.animotron.manipulator.QCAVector;
import org.animotron.statement.operator.Operator;
import org.animotron.statement.operator.Predicate;
import org.animotron.statement.operator.Utils;
import org.animotron.statement.query.GET;
import org.jetlang.channels.Subscribable;
import org.jetlang.core.DisposingExecutor;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;

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

	@Override
	public boolean filter(PFlow pf, Relationship op, Node ref) throws InterruptedException, IOException {
		System.out.println("==================================================");
		System.out.println("EQ op "+op+" ref "+ref);
		//XXX: fix
		Node theNode = Utils.getSingleREF(op.getEndNode());

		Set<Node> thes = new FastSet<Node>();
		thes.add(theNode);
		
		final PipedOutput<QCAVector> out = new PipedOutput<QCAVector>();
		PipedInput<QCAVector> in = out.getInputStream();
		
		PFlow pflow = new PFlow(new PFlow(Evaluator._), pf.getVector().question(op));
		
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
		
		GET._.getBySELF(pflow, ref, thes);
		pflow.done();
		
		if (!in.hasNext()) return false;
		
		List<QCAVector> actual = new FastList<QCAVector>();
		List<QCAVector> expected = new FastList<QCAVector>();

		//System.out.println("Eval actual");
		for (QCAVector res : in) { 
			in = Evaluator._.execute(new PFlow(pf), res);
			for (QCAVector e : in) {
				actual.add(e);
				//System.out.println("actual "+e);
			}
		}

		//System.out.println("Eval expected");
		in = Evaluator._.execute(new PFlow(pf), op.getEndNode());
		for (QCAVector e : in) {
			expected.add(e);
			//System.out.println("expected "+e);
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