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
package org.animotron.statement.operator;

import org.animotron.Executor;
import org.animotron.manipulator.OnQuestion;
import org.animotron.manipulator.PFlow;
import org.animotron.statement.Statement;
import org.animotron.statement.Statements;
import org.jetlang.channels.Subscribable;
import org.jetlang.core.DisposingExecutor;
import org.neo4j.graphdb.Relationship;

import java.util.Arrays;
import java.util.Iterator;

import static org.neo4j.graphdb.Direction.OUTGOING;

/**
 * Operation 'EACH'. Run sub-sequence per element.
 * 
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 */
public class EACH extends Operator implements Evaluable {

	public static final EACH _ = new EACH();

	private EACH() { super("each"); }

	@Override
	public Subscribable<PFlow> onCalcQuestion() {
		return question;
	}

	//XXX: out of date!
	private OnQuestion question = new OnQuestion() {

		@Override
		public void onMessage(final PFlow pf) {
			System.out.println("EACH");
			
			Subscribable<Relationship[]> onContext = new Subscribable<Relationship[]>() {
				@Override
				public void onMessage(Relationship[] context) {
					System.out.println("EACH message context "+Arrays.toString(context));
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

					PFlow nextPF = new PFlow(pf, r);
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