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
package org.animotron.statement.combinator;

import javolution.util.FastSet;
import org.animotron.expression.JExpression;
import org.animotron.graph.index.Order;
import org.animotron.io.Pipe;
import org.animotron.manipulator.OnQuestion;
import org.animotron.manipulator.PFlow;
import org.animotron.manipulator.QCAVector;
import org.animotron.statement.Statement;
import org.animotron.statement.Statements;
import org.animotron.statement.link.LINK;
import org.animotron.statement.operator.AN;
import org.animotron.statement.operator.DEF;
import org.animotron.statement.operator.Evaluable;
import org.animotron.statement.operator.Query;
import org.animotron.statement.operator.Shift;
import org.animotron.statement.operator.Utils;
import org.animotron.statement.value.AbstractValue;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.index.IndexHits;

import java.io.IOException;
import java.util.LinkedList;
import java.util.Set;

/**
 * Operation 'EACH'.
 * 
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 */
public class EACH extends Combinator {

	public static final EACH _ = new EACH();

	private static boolean debug = false; 

	private EACH() { super("each"); }

	@Override
	public OnQuestion onCalcQuestion() {
		return new Calc();
	}
	
	class Calc extends OnQuestion {
	
		@Override
		public void act(final PFlow pf) throws IOException {
			if (debug) { 
				System.out.println("EACH EACH EACH EACH");
				System.out.println(pf.getVector());
				System.out.println("EACH EACH EACH EACH");
			}
			
			IndexHits<Relationship> elements = Order._.queryDown(pf.getOPNode());
			try {
				Set<QCAVector> set = new FastSet<QCAVector>();
				while (elements.hasNext()) {
					Relationship element = elements.next();
					if (elements.hasNext()) {
						Pipe pipe = Utils.getTheRelationships(pf, pf.getVector().question2(element));
					
						QCAVector r;
						while ((r = pipe.take()) != null)
							set.add(r);

					} else {
						for (QCAVector r : set) {
//			                Statement qS = Statements.relationshipType(r.getQuestion());
//							if (qS instanceof Shift && r.getAnswer().isType(AN._)) {
////								IndexHits<Relationship> hits = Order._.context(r.getAnswerEndNode());
////								try {
////									for (Relationship h : hits) {
////										QCAVector rr;
////										
////										//check if it evaluable
////										Statement s = Statements.relationshipType(h);
////										if (s != null && (s instanceof Query || s instanceof Evaluable)) {
//											Pipe p = Utils.eval(pf.getController(), r);
//											QCAVector v;
//											while ((v = p.take()) != null) {
//												QCAVector rr = new QCAVector(element, new QCAVector(pf.getOP(), v));
//												pf.sendAnswer(rr);
//											}
////										} else {
////											rr = new QCAVector(element, new QCAVector(pf.getOP(), r, h));
////											pf.sendAnswer(rr);
////										}
////									}
////								} finally {
////									hits.close();
////								}
//							} else {
								QCAVector rr = new QCAVector(element, new QCAVector(pf.getOP(), r));
								pf.sendAnswer(rr);
//							}
						}
					}
				}

				//process(pf, sets, 1, null);

			} finally {
				elements.close();
			}
		}
	}

	@SuppressWarnings("unused")
	private void processByBuildSubgraph(PFlow pf, LinkedList<Relationship> sets, int pos, Relationship[] res) {
		if (pos > sets.size()) {

            Relationship r = res[res.length-1];
            Relationship the = null;
			for (int i = res.length-2; i >= 0; i--) {
                Statement s = Statements.relationshipType(res[i]);
                if (s instanceof AbstractValue) {
                    // TODO analyze more complex expressions
                    IndexHits<Relationship> subelements = Order._.queryDown(res[i].getEndNode());
                    Object[][] bind = new Object[subelements.size() + 1][];
                    try {
                        int n = 0;
                        for (Relationship x : subelements) {
                            bind[n] = JExpression._(x);
                            n++;
                        }
                        bind[n] = JExpression._(r);
                        r = new JExpression(JExpression._(s, ((AbstractValue) s).reference(res[i].getEndNode()), bind));
                    } finally {
                        subelements.close();
                    }
                } else {
                    the = new JExpression(JExpression._(DEF._, JExpression._(res[i])));
                    r = new JExpression(JExpression._(AN._, the, JExpression._(r)));
                }
            }

			pf.sendAnswer(r);
			return;
		}

		Relationship[] rs = new Relationship[pos];

		if (pos > 1) {
			System.arraycopy(res, 0, rs, 0, res.length);
		}

		Relationship rship = sets.get( sets.size()-pos );

		if (rship.isType(LINK._)) {
			IndexHits<Relationship> subelements = Order._.queryDown(rship.getEndNode());
			try {
				for (Relationship r : subelements) {
					rs[pos-1] = r;
					processByBuildSubgraph(pf, sets, pos+1, rs);
				}
			} finally {
				subelements.close();
			}
		} else {
			rs[pos-1] = rship;
			processByBuildSubgraph(pf, sets, pos+1, rs);
		}
	}
}