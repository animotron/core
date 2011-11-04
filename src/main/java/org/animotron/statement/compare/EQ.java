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
import org.animotron.io.PipedInput;
import org.animotron.manipulator.Evaluator;
import org.animotron.manipulator.PFlow;
import org.animotron.statement.operator.Operator;
import org.animotron.statement.operator.Predicate;
import org.animotron.statement.query.GET;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;

import java.io.IOException;
import java.util.List;

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
		String name = (String) reference(op);

		Relationship have = GET._.getBySELF(pf, ref, name);
		if (have == null) return false;
		
		List<Relationship> actual = new FastList<Relationship>();
		List<Relationship> expected = new FastList<Relationship>();

		System.out.println("Eval actual");
		PipedInput<Relationship[]> in = Evaluator._.execute(new PFlow(pf), have.getEndNode());
		for (Relationship[] e : in) {
			actual.add(e[0]);
			System.out.println("actual "+e);
		}

		System.out.println("Eval expected");
		in = Evaluator._.execute(new PFlow(pf), op.getEndNode());
		for (Relationship[] e : in) {
			expected.add(e[0]);
			System.out.println("expected "+e);
		}
		
		if (actual.size() == 1 && expected.size() == 1) {
			Relationship e = actual.get(0);
			Relationship g = expected.get(0);
			
			if (e.isType(g.getType())
				&& e.getEndNode().equals(g.getEndNode()))
				
				return true;
		}

		return false;
	}
}