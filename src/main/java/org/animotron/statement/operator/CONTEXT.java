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
package org.animotron.statement.operator;

import javolution.util.FastList;
import org.animotron.graph.index.Order;
import org.animotron.manipulator.Evaluator;
import org.animotron.manipulator.OnContext;
import org.animotron.manipulator.OnQuestion;
import org.animotron.manipulator.PFlow;
import org.animotron.manipulator.QCAVector;
import org.animotron.statement.link.AbstractLink;
import org.animotron.statement.query.ALL;
import org.animotron.statement.query.ANY;
import org.animotron.statement.query.GET;
import org.animotron.statement.query.PREFER;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.index.IndexHits;

import java.io.IOException;
import java.util.List;

/**
 * Operation 'THIS'. Reference to the closest instance in PFlow.
 * 
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 */
public class CONTEXT extends AbstractLink implements Reference, Evaluable {

	public static final CONTEXT _ = new CONTEXT();

	private CONTEXT() { super("context"); }
	
	private static boolean debug = false;
	
    @Override
	public OnQuestion onCalcQuestion() {
		return new Calc();
    }
    
    class Calc extends OnQuestion {
	
		@Override
		public void act(final PFlow pf) throws IOException {
			
			System.out.println("CONTEXT");
			
			if (!Utils.results(pf)) {
				if (Utils.haveContext(pf)) {
					OnContext onContext = new OnContext() {
						@Override
						public void onMessage(QCAVector vector) {
							super.onMessage(vector);
							
							if (vector == null)
								return;
							
							System.out.println(vector);
							
							FastList<QCAVector> list = FastList.newInstance();
							try {
								if (pf.getVector().getContext() != null)
									list.addAll( vector.getContext() );
								
								search(pf, list, null);
								
							} finally {
								FastList.recycle(list);
							}
						}
					};
					//pf.answerChannel().subscribe(onContext);
					
					Evaluator.sendQuestion(pf.getController(), onContext, pf.getVector(), pf.getOPNode(), false);
					onContext.isDone();
				} else {

					FastList<QCAVector> list = FastList.newInstance();
					try {
						if (pf.getVector().getContext() != null)
							list.addAll( pf.getVector().getContext() );
						
						search(pf, list, null);
						
					} finally {
						FastList.recycle(list);
					}
				}
			}
		}
    }
	
	private boolean search(final PFlow pf, List<QCAVector> cs, Relationship prevAnswer) {
		if (cs != null) {
			for (QCAVector next : cs) {
				if (debug) 
					System.out.println(next);
				
				FastList<QCAVector> list = FastList.newInstance();
				try {
					final Relationship toCheck = next.getQuestion();
					if (
							toCheck.isType(ALL._) || 
							toCheck.isType(ANY._) || 
							toCheck.isType(PREFER._) || 
							toCheck.isType(AN._)
						) {

						if (next.getUnrelaxedAnswer() != null) {
							IndexHits<Relationship> hits = Order._.context(toCheck.getEndNode());
							try {
								for (Relationship r : hits) {
									System.out.println(r);
									pf.sendAnswer(pf.getVector().answered(r));//, next.getContext()
								}
								return true;
							} finally {
								hits.close();
							}
						}
					} else if ( toCheck.isType(GET._) ) {

						if (next.getUnrelaxedAnswer() != null) {
							IndexHits<Relationship> hits = Order._.context(next.getAnswerEndNode());
							try {
								for (Relationship r : hits) {
									System.out.println(r);
									pf.sendAnswer(pf.getVector().answered(r));//, next.getContext()
								}
								return true;
							} finally {
								hits.close();
							}
						}
					}
					if (next.getContext() != null)
						list.addAll(next.getContext());
					
					if (search(pf, list, next.getAnswer()))
						return true;
					
				} finally {
					FastList.recycle(list);
				}
			}
		}
		return false;
	}
}