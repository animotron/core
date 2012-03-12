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
package org.animotron.statement.math;

import java.io.IOException;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;

import javolution.util.FastList;

import org.animotron.expression.JExpression;
import org.animotron.manipulator.PFlow;
import org.animotron.statement.value.VALUE;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;

import static org.animotron.expression.JExpression.value;

/**
 * Math instruction 'SUM'. (aka summation)
 * 
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 */
public class SUM extends MathInstruction {

	public static final SUM _ = new SUM();

	private SUM() {
		super("+");
	}

	protected Relationship execute(final PFlow pf, AnimObject a, AnimObject b) throws IOException {
		System.out.println("+");

		List<Relationship> As = a.getElements(pf);
		List<Relationship> Bs = b.getElements(pf);

		System.out.println("As = ");
		System.out.println(Arrays.toString(As.toArray()));
		System.out.println("Bs = ");
		System.out.println(Arrays.toString(Bs.toArray()));

		if (As.size() == Bs.size()) {
			List<Relationship> eq = new FastList<Relationship>();

			Iterator<Relationship> it = As.iterator();
			while (it.hasNext()) {
				Relationship r = it.next();
				if (r.isType(VALUE._)) {
					Node n = r.getEndNode();
					
					Iterator<Relationship> Bit = Bs.iterator();
					while (Bit.hasNext()) {
						Relationship rr = Bit.next();
						if (rr.isType(VALUE._)) {
							if (rr.getEndNode().equals(n)) {
								eq.add(rr);
								
								it.remove();
								Bit.remove();

								break;
							}
						}
					}
					
				} else if (Bs.contains(r)) {
					eq.add(r);

					it.remove();
					Bs.remove(Bs.indexOf(r));
				}
			}

			if (As.size() == 1 && As.size() == Bs.size()) {
				eq.add(execute(pf, As.get(0), Bs.get(0)));
			}

			System.out.println(Arrays.toString(eq.toArray()));

			return new AnimObject(MUL._, eq);
		}
		return new AnimObject(SUM._, a, b);
	}

	protected Relationship execute(final PFlow pf, AnimObject a) throws IOException {
		List<Relationship> elements = a.getElements(pf);
		
		//System.out.println(Arrays.toString(elements.toArray()));
		
		Relationship res = null;
		for (Relationship r : elements) {
			if (res == null)
				res = r;
			else
				res = execute(pf, res, r);
		}
		
		return res;
	}
	
	private Relationship execute(final PFlow pf, Relationship a, Relationship b) throws IOException {
		if (a.isType(VALUE._) && b.isType(VALUE._)) {
			Number Na = VALUE.number(VALUE._.reference(a));
			Number Nb = VALUE.number(VALUE._.reference(b));

			Number result;
			if ((Na instanceof Long || Na instanceof Integer) 
					&& (Nb instanceof Long || Nb instanceof Integer)) {
				result = Na.longValue() + Nb.longValue();
			
			} else {
				result = Na.doubleValue() + Nb.doubleValue();
			}

			return new JExpression(value(result));
		
		} else  if (a instanceof AnimObject && b instanceof AnimObject) {
			return execute(pf, (AnimObject)a, (AnimObject)b);
		}
		return new AnimObject(SUM._, a, b);
	}
}
