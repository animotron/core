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
package org.animotron.statement.combinator;

import org.animotron.Executor;
import org.animotron.exception.AnimoException;
import org.animotron.manipulator.QCAVector;
import org.animotron.manipulator.OnQuestion;
import org.animotron.manipulator.PFlow;
import org.animotron.statement.Statement;
import org.animotron.statement.Statements;
import org.animotron.statement.operator.Query;
import org.animotron.statement.operator.Reference;
import org.jetlang.channels.Subscribable;
import org.jetlang.core.DisposingExecutor;
import org.neo4j.graphdb.Relationship;

import java.util.Arrays;
import java.util.Iterator;

import static org.neo4j.graphdb.Direction.OUTGOING;

/**
 * Operation 'MAP'.
 * 
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 */
public class MAP extends Combinator {

	public static final MAP _ = new MAP();

	private MAP() { super("map"); }

	@Override
	public Subscribable<PFlow> onCalcQuestion() {
		return question;
	}

	private OnQuestion question = new OnQuestion() {

		@Override
		public void onMessage(final PFlow pf) {
			System.out.println("MAP");
			
			Subscribable<QCAVector> onContext = new Subscribable<QCAVector>() {
				@Override
				public void onMessage(QCAVector context) {
					System.out.println("MAP vector "+context);
					if (context == null) {
						pf.countDown();
						return;
					}
				}

				@Override
				public DisposingExecutor getQueue() {
					return Executor.getFiber();
				}
			};
			
			pf.answer.subscribe(onContext);
			
			int count = 0;

			Iterator<Relationship> it = pf.getOPNode().getRelationships(OUTGOING).iterator();
			while (it.hasNext()) {
				Relationship r = it.next();
				Statement s = Statements.relationshipType(r);
				
				if (s instanceof Reference || s instanceof Query) {
					Subscribable<PFlow> onQuestion = pf.getManipulator().onQuestion(r);

					PFlow nextPF;
					try {
						nextPF = new PFlow(pf, r);
					} catch (AnimoException e) {
						continue;
					}
					nextPF.question.subscribe(onQuestion);
					
					nextPF.question.publish(nextPF);
					
					count++;
				}
			}
			
			if (count == 0)
				pf.done();
			else {
				pf.await();
				pf.done();
			}
		}
	};
}