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
import org.animotron.statement.Statement;
import org.animotron.statement.Statements;
import org.animotron.statement.operator.Evaluable;
import org.animotron.statement.operator.Operator;
import org.animotron.statement.operator.Predicate;
import org.animotron.statement.operator.Query;
import org.animotron.statement.query.GET;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.index.IndexHits;

import java.io.IOException;
import java.util.List;
import java.util.Set;

import static org.animotron.graph.AnimoGraph.getORDER;

/**
 * Compare operator 'WITH'.
 * 
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 */
public class WITH extends Operator implements Predicate {
	
	public static final WITH _ = new WITH();
	
	private WITH() { super("with"); }

	@Override
	public boolean filter(PFlow pf, Relationship start_op, Relationship op, Node ref) throws InterruptedException, IOException {
		
		System.out.println("==================================================");
		System.out.println("WITH op "+op+" ref "+ref);
		//XXX: fix
		String name = name(op);

		Set<Relationship> haveSet = GET._.getByTraversal(GET.getHaveAtPFlow(pf, name), op, ref, name);
		if (haveSet.isEmpty()) return false;
		
		List<Relationship> actual = new FastList<Relationship>();
		List<Relationship> expected = new FastList<Relationship>();

		PipedInput in = null;
		
		System.out.println("Eval actual");
		for (Relationship have : haveSet) {
			in = Evaluator._.execute(start_op, have.getEndNode());
			for (Object e : in) {
				actual.add((Relationship) e);
				System.out.println("actual "+e);
			}
		}

		System.out.println("Eval expected");
		in = Evaluator._.execute(start_op, op.getEndNode());
		for (Object e : in) {
			expected.add((Relationship) e);
			System.out.println("expected "+e);
		}
		
		if (actual.size() >= 1 && expected.size() == 1) {
			Relationship g = expected.get(0);
			
			//XXX: finish
			List<Relationship> l = evaluable(start_op, g.getEndNode());
			if (l.size() == 1)
				g = l.get(0);
			else if (l.size() > 1) 
				System.out.println("DON'T KNOW WHAT TO DO, get alot of results @WITH");

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
	
	private List<Relationship> evaluable(Relationship start_op, Node node) throws InterruptedException, IOException {
		List<Relationship> list = new FastList<Relationship>();
		
		IndexHits<Relationship> q = getORDER().query(node);
		try {
			for (Relationship i : q) {
				Statement s = Statements.relationshipType(i.getType());
    			if (s instanceof Query || s instanceof Evaluable) {
    				System.out.println("+++++++++++++++++++++++++++++++++++++++++ get evaluable");
    				PipedInput in = Evaluator._.execute(start_op, i);
    				for (Object e : in) {
    					list.add((Relationship) e);
    					System.out.println("get from Evaluator "+e);
    				}
    			} else {
    				list.add(i);
    			}
			}
		} finally {
			q.close();
		}

		
		return list;
	}
}
