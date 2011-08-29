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
import org.animotron.Expression;
import org.animotron.exception.AnimoException;
import org.animotron.manipulator.OnQuestion;
import org.animotron.manipulator.PFlow;
import org.animotron.statement.AbstractStatement;
import org.jetlang.channels.Subscribable;
import org.jetlang.core.DisposingExecutor;
import org.neo4j.graphdb.Relationship;

import java.util.concurrent.atomic.AtomicInteger;

/**
 * Operation 'COUNT'. Count elements.
 * 
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 */
public class COUNT extends AbstractStatement implements Evaluable, Cachable {

	public static final COUNT _ = new COUNT();

	private COUNT() { super("count"); }

	@Override
	public Subscribable<PFlow> onCalcQuestion() {
		return question;
	}

	private OnQuestion question = new OnQuestion() {

		@Override
		public void onMessage(final PFlow pf) {
			//System.out.println("COUNT");
			
			Subscribable<Relationship> onContext = new Subscribable<Relationship>() {
				
				AtomicInteger value = new AtomicInteger(0);
				
				@Override
				public void onMessage(Relationship context) {
					if (context == null) {
						//XXX: optimize
						Expression r;
						try {
							r = new Expression(Expression._(Q._, "N"+value.get()));
						} catch (AnimoException e) {
							pf.sendException(e);
							return;
						}
						pf.sendAnswer(r);
						pf.done();
						return;
					}
					
			        value.getAndIncrement();
				}

				@Override
				public DisposingExecutor getQueue() {
					return Executor.getFiber();
				}
			};
			
			pf.answer.subscribe(onContext);
		}
	};
}