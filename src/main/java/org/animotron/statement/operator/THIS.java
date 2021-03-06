/*
 *  Copyright (C) 2011-2013 The Animo Project
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
import javolution.util.FastSet;
import org.animotron.graph.Properties;
import org.animotron.graph.index.Order;
import org.animotron.io.Pipe;
import org.animotron.manipulator.OnQuestion;
import org.animotron.manipulator.PFlow;
import org.animotron.manipulator.QCAVector;
import org.animotron.statement.query.ALL;
import org.animotron.statement.query.ANY;
import org.animotron.statement.query.GET;
import org.animotron.statement.query.PREFER;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.index.IndexHits;

import java.util.List;
import java.util.Set;

/**
 * Operation 'THIS'. Reference to the closest instance in PFlow.
 * 
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 */
public class THIS extends Operator implements Reference, Evaluable {

	public static final THIS _ = new THIS();

	private THIS() { super("this"); }
	
	private static boolean debug = true;
	
    @Override
	public OnQuestion onCalcQuestion() {
		return new Calc();
    }
    
    class Calc extends OnQuestion {
	
		@Override
		public void act(final PFlow pf) {
			
			Relationship op = pf.getOP();
			
			final Set<Node> thes = new FastSet<Node>();
			Pipe p = AN.getREFs(pf, pf.getVector());
			QCAVector theNode;
			while ((theNode = p.take()) != null) {
				thes.add(theNode.getClosest().getEndNode());
			}

			if (debug) 
				Utils.debug(THIS._, op, thes);
			
			if (thes.isEmpty())
				return;
			
			if (!Utils.results(pf)) {
				FastList<QCAVector> list = FastList.newInstance();
				try {
					if (pf.getVector().getContext() != null)
						list.addAll( pf.getVector().getContext() );
					
					search(pf, thes, list, null);
					
				} finally {
					FastList.recycle(list);
				}
			}
		}
    }
	
	private boolean search(final PFlow pf, final Set<Node> thes, List<QCAVector> cs, Relationship prevAnswer) {
		if (cs != null) {
			for (QCAVector next : cs) {
				if (debug) 
					System.out.println(next);
				
				FastList<QCAVector> list = FastList.newInstance();
				try {
					Relationship toCheck = next.getQuestion();
					if (toCheck.isType(THIS._)) {
						if (thes.isEmpty()) {
						} else {
							Node n = Utils.getByREF(toCheck).getEndNode();
							if (thes.contains( n )) {
								pf.sendAnswer(pf.getVector().answered(next.getUnrelaxedAnswer())); //, next.getContext()
								return true;
							}
						}
					} else if (toCheck.isType(ALL._) || toCheck.isType(ANY._) || toCheck.isType(PREFER._) || toCheck.isType(GET._)) {
						Node n = Utils.getByREF(toCheck).getEndNode();
						
						if (debug) System.out.println(n);
						
						if (thes.contains( n )) {
							if (debug) System.out.println("answer "+next.getUnrelaxedAnswer());
							pf.sendAnswer(pf.getVector().answered(next.getUnrelaxedAnswer(), next.getContext())); //, next.getContext()
							return true;
						}

					} else if (toCheck.isType(AN._)) {
						if (next.hasAnswer()) {
							Node n = Utils.getByREF(toCheck).getEndNode();
							if (thes.contains( n )) {
								//find highest by pseudo-IS
								if (next.getContext() != null && next.getContext().size() == 1) {
									Relationship ths = next.getAnswer();
									QCAVector v = next.getContext().get(0);
									Node thisNode = next.getQuestion().getStartNode();
									while (thisNode != null ) {
										if (v.getQuestion().isType(AN._) && v.hasAnswer()) {
											Node nextNode = v.getUnrelaxedAnswer().getEndNode();
											if (nextNode.equals(thisNode)) {
												
												thisNode = nextNode;
												if (!Properties.DEFID.has(thisNode))
													break;
												
												ths = v.getAnswer();

												if (v.getContext() == null || v.getContext().size() != 1)
													break;

												v = next.getContext().get(0);
											}
										}
										break;
									}
									pf.sendAnswer(pf.getVector().answered(ths));//, next.getContext()
								}
								
								IndexHits<Relationship> hits = Order._.context(toCheck.getEndNode());
								try {
									for (Relationship r : hits) {
										pf.sendAnswer(pf.getVector().answered(r));//, next.getContext()
									}
									return true;
								} finally {
									hits.close();
								}
							}
						}
					}
					if (next.getContext() != null)
						list.addAll(next.getContext());
					
					if (search(pf, thes, list, next.getAnswer()))
						return true;
					
				} finally {
					FastList.recycle(list);
				}
			}
		}
		return false;
	}
}