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

import java.util.Arrays;
import java.util.List;

import org.animotron.manipulator.PFlow;
import org.animotron.manipulator.Evaluator;
import org.animotron.operator.AbstarctOperator;
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
public class WITH extends AbstarctOperator implements Predicate {
	
	public static final WITH _ = new WITH();
	
	private WITH() { super("with", "animo/compare/with"); }

	@Override
	public void filter(Relationship op, PFlow ch, boolean isLast) {
		//XXX: out.subscribeFilter(op, this);
	}

	@Override
	public boolean filter(Relationship op, Node ref) {
		
		System.out.println("WITH op "+op);
		//XXX: fix
		String name = name(op);

		Relationship have = GET._.get(ref, name);
		if (have == null) return false;
		
		//TODO: improve
		System.out.println("================================== actual");
		List<Relationship> actual = Evaluator._.evalGetResult(have);
		System.out.println("================================== actual result");
		System.out.println(Arrays.toString(actual.toArray()));
		System.out.println("================================== expected");
		List<Relationship> expected = Evaluator._.evalGetResult(op.getEndNode());
		System.out.println("================================== expected result");
		System.out.println(Arrays.toString(expected.toArray()));
		
		if (actual.size() == 1 && expected.size() == 1) {
			Relationship e = actual.get(0);
			Relationship g = expected.get(0);
			if (
			       e.getType().name().equals(g.getType().name()) 
				&& e.getEndNode().equals(g.getEndNode()))
				
				return true;
		}

		return false;
	}
	
}
