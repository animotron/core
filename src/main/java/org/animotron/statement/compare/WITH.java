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
import org.animotron.graph.index.Order;
import org.animotron.io.PipedInput;
import org.animotron.manipulator.ACQVector;
import org.animotron.manipulator.Evaluator;
import org.animotron.manipulator.PFlow;
import org.animotron.statement.Statement;
import org.animotron.statement.Statements;
import org.animotron.statement.operator.Evaluable;
import org.animotron.statement.operator.Operator;
import org.animotron.statement.operator.Predicate;
import org.animotron.statement.operator.Query;
import org.animotron.statement.operator.REF;
import org.animotron.statement.operator.Utils;
import org.animotron.statement.query.GET;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.index.IndexHits;

import java.io.IOException;
import java.util.List;
import java.util.Set;

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
	public boolean filter(PFlow pf, Relationship op, Node ref) throws InterruptedException, IOException {
		
		System.out.println("==================================================");
		System.out.println("WITH op "+op+" ref "+ref);
		//XXX: fix
		Node theNode = Utils.getSingleREF(op.getEndNode());

		Set<ACQVector> haveSet = GET._.get(pf, ref, theNode, null, null);
		if (haveSet == null || haveSet.isEmpty()) return false;
		
		List<ACQVector> actual = new FastList<ACQVector>();
		List<ACQVector> expected = new FastList<ACQVector>();

		PipedInput<ACQVector> in;
		
		//System.out.println("Eval actual");
		for (ACQVector have : haveSet) {
			in = Evaluator._.execute(new PFlow(pf), have.getAnswer().getEndNode());
			for (ACQVector e : in) {
				actual.add(e);
				//System.out.println("actual "+e);
			}
		}

		//System.out.println("Eval expected");
		in = Evaluator._.execute(new PFlow(pf), op.getEndNode());
		for (ACQVector e : in) {
			expected.add(e);
			//System.out.println("expected "+r);
		}
		
		if (actual.size() >= 1 && expected.size() == 1) {
			ACQVector g = expected.get(0);
			
			//XXX: finish
			List<ACQVector> l = evaluable(pf, g.getAnswer().getEndNode());
			if (l.size() == 1)
				g = l.get(0);
			else if (l.size() > 1) {
				
				System.out.println("DON'T KNOW WHAT TO DO, get alot of results @WITH");
//				for (ACQVector r : l) {
//					System.out.println(""+r+" "+r.getType());
//				}
			}

			//System.out.println("***** expected = "+g.getEndNode());
			
			for (ACQVector e : actual) {
				//System.out.println("***** actual = "+e.getEndNode());
				if (e.getAnswer().isType(g.getAnswer().getType())
					&& e.getAnswer().getEndNode().equals(g.getAnswer().getEndNode()))
					
					return true;
			}
		}

		return false;
	}
	
	private List<ACQVector> evaluable(PFlow pf, Node node) throws InterruptedException, IOException {
		List<ACQVector> list = new FastList<ACQVector>();
		
		IndexHits<Relationship> q = Order.queryDown(node);
		try {
			for (Relationship i : q) {
				if (i.isType(REF._)) continue;
				
				Statement s = Statements.relationshipType(i);
    			if (s instanceof Query || s instanceof Evaluable) {
    				//System.out.println("+++++++++++++++++++++++++++++++++++++++++ get evaluable");
    				PipedInput<ACQVector> in = Evaluator._.execute(pf, i);
    				for (ACQVector e : in) {
    					list.add(e);
    					//System.out.println("get from Evaluator "+r);
    				}
    			} else {
    				list.add(new ACQVector(null, i));
    			}
			}
		} finally {
			q.close();
		}

		
		return list;
	}
}
