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

import javolution.util.FastList;
import org.animotron.manipulator.PFlow;
import org.neo4j.graphdb.Relationship;

import java.io.IOException;
import java.util.Arrays;
import java.util.List;

import static org.animotron.statement.operator.VALUE.expression;

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

	protected Relationship execute(final PFlow pf, Relationship a) throws IOException {
		return a;
	}

	protected Relationship execute(Number Na, Number Nb) throws IOException {
		Number result;
		if ((Na instanceof Long || Na instanceof Integer) 
				&& (Nb instanceof Long || Nb instanceof Integer)) {
			result = Na.longValue() + Nb.longValue();
		
		} else {
			result = Na.doubleValue() + Nb.doubleValue();
		}

		System.out.println(""+Na.doubleValue()+" + "+Nb.doubleValue()+" = "+result);

		return expression(result);
	}
	
	protected AnimObject execute(final PFlow pf, AnimObject a, AnimObject b) throws IOException {
		if (!(a.op == b.op && b.op == MUL._))
			throw new IOException("\ncan't '"+name()+"'\n"+a+" "+b);
		
		List<Relationship> As = new FastList<Relationship>( a.getElements(pf) );
		List<Relationship> Bs = new FastList<Relationship>( b.getElements(pf) );

		System.out.println("As = ");
		System.out.println(Arrays.toString(As.toArray()));
		System.out.println("Bs = ");
		System.out.println(Arrays.toString(Bs.toArray()));

		if (As.size() == Bs.size()) {
			List<Relationship> eq = new FastList<Relationship>();
			List<Relationship> nm = new FastList<Relationship>();
			
			findEqNm(As, Bs, eq, nm);

			System.out.println("As = ");
			System.out.println(Arrays.toString(As.toArray()));
			System.out.println("Bs = ");
			System.out.println(Arrays.toString(Bs.toArray()));

			System.out.println("eq = ");
			System.out.println(Arrays.toString(eq.toArray()));
			System.out.println("nm = ");
			System.out.println(Arrays.toString(nm.toArray()));
			
			if (As.isEmpty() && Bs.isEmpty()) {
				Relationship res = null;
				for (Relationship r : nm) {
					if (res == null)
						res = r;
					else {
						res = execute(pf, res, r);
						if (res == null)
							throw new IOException("\ncan't '"+name()+"'\n"+a+" "+b);
					}
				}
				eq.add(res);
				return new AnimObject(pf, MUL._, eq);
			}
		}
		return new AnimObject(pf, this, a, b);
//		throw new IOException("can't '"+name()+"'\n "+a+" "+b);
	}
}