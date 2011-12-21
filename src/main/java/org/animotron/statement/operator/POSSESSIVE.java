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

import javolution.util.FastSet;
import org.animotron.Executor;
import org.animotron.manipulator.OnQuestion;
import org.animotron.manipulator.PFlow;
import org.animotron.manipulator.QCAVector;
import org.animotron.statement.Suffix;
import org.jetlang.channels.Subscribable;
import org.jetlang.core.DisposingExecutor;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;

import java.util.Set;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 */
public class POSSESSIVE extends Operator implements Suffix {

	public static final POSSESSIVE _ = new POSSESSIVE();

	private POSSESSIVE() { super("'s"); }
	
	public OnQuestion onCalcQuestion() {
		return question;
	}
	
	private OnQuestion question = new OnQuestion() {

		@Override
		public void onMessage(final PFlow pf) {
			final Relationship op = pf.getOP();
			
			final Set<Node> thes = new FastSet<Node>(); 
			
			for (QCAVector theNode : AN.getREFs(pf, op)) {
				thes.add(theNode.getAnswer().getEndNode());
			}
			Utils.debug(POSSESSIVE._, op, thes);

			Subscribable<QCAVector> onContext = new Subscribable<QCAVector>() {
				@Override
				public void onMessage(QCAVector vector) {
					System.out.println("GET ["+op+"] vector "+vector);
					
					if (vector == null) {
						pf.countDown();
						return;
					}

					//System.out.println("checkSuffixes checkSuffixes checkSuffixes "+res);
					
					Node node = vector.getAnswer().getEndNode();
					
					for (Relationship r : Utils.td_eval_IS.traverse(node).relationships()) {
						//System.out.println(r);
						if (thes.contains(r.getEndNode())) {
							pf.sendAnswer(r);
							break;
						}
					}
				}

				@Override
				public DisposingExecutor getQueue() {
					return Executor.getFiber();
				}
			};
			pf.answerChannel().subscribe(onContext);

			if (Utils.haveContext(pf)) {
				super.onMessage(pf);
			} else {
				
//				for (QCAVector vector : pf.getPFlowPath()) {
//					//System.out.println("CHECK PFLOW "+vector);
//					Set<QCAVector> rSet = get(pf, op, vector, thes, suffixes, visitedREFs);
//					if (rSet != null) {
//						for (QCAVector v : rSet) {
//							pf.sendAnswer(v, AN._);
//						}
//						break;
//					}
//				}
			}
		}
	};
}