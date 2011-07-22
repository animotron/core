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
package org.animotron.operator.compare;

import static org.animotron.Properties.RID;
import static org.animotron.graph.AnimoGraph.getDb;

import java.io.IOException;
import java.util.List;

import javolution.util.FastList;

import org.animotron.graph.RelationshipTypes;
import org.animotron.io.PipedInput;
import org.animotron.manipulator.Evaluator;
import org.animotron.operator.AbstractOperator;
import org.animotron.operator.Predicate;
import org.animotron.operator.query.GET;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;

/**
 * Compare operator 'WITH'.
 * 
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 */
public class WITH extends AbstractOperator implements Predicate {
	
	public static final WITH _ = new WITH();
	
	private WITH() { super("with", "animo/compare/with"); }

	@Override
	public boolean filter(Relationship start_op, Relationship op, Node ref) throws InterruptedException, IOException {
		
		System.out.println("==================================================");
		System.out.println("WITH op "+op+" ref "+ref);
		//XXX: fix
		String name = name(op);

		Relationship have = GET._.get(ref, name);
		if (have == null) return false;
		
		List<Relationship> actual = new FastList<Relationship>();
		List<Relationship> expected = new FastList<Relationship>();

		System.out.println("Eval actual");
		PipedInput in = Evaluator._.execute(start_op, have.getEndNode());
		for (Object e : in) {
			actual.add((Relationship) e);
			System.out.println("actual "+e);
		}

		System.out.println("Eval expected");
		in = Evaluator._.execute(start_op, op.getEndNode());
		for (Object e : in) {
			expected.add((Relationship) e);
			System.out.println("expected "+e);
		}
		
		if (actual.size() >= 1 && expected.size() == 1) {
			Relationship g = expected.get(0);

			System.out.println("***** expected = "+g.getEndNode());
			
			for (Relationship e : actual) {
				System.out.println("***** actual = "+e.getEndNode());
				if (   e.getType().name().equals(g.getType().name()) 
					&& e.getEndNode().equals(g.getEndNode()))
					
					return true;
			}
		}

		return false;
	}
}
