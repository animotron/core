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
package org.animotron.statement.operator.combinator;

import org.animotron.expression.Expression;
import org.animotron.expression.JExpression;
import org.animotron.graph.OrderIndex;
import org.animotron.manipulator.OnQuestion;
import org.animotron.manipulator.PFlow;
import org.animotron.statement.link.LINK;
import org.jetlang.channels.Subscribable;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.index.IndexHits;

import java.util.Arrays;
import java.util.LinkedList;

/**
 * Operation 'EACH'.
 * 
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 */
public class EACH extends Combinator {

	public static final EACH _ = new EACH();

	private EACH() { super("each"); }

	@Override
	public Subscribable<PFlow> onCalcQuestion() {
		return question;
	}

	private OnQuestion question = new OnQuestion() {

		@Override
		public void onMessage(final PFlow pf) {
			System.out.println("EACH");
			
			LinkedList<Relationship> sets = new LinkedList<Relationship>();
			
			IndexHits<Relationship> elements = OrderIndex.queryDown(pf.getOPNode());
			try {
				while (elements.hasNext()) {
					sets.add(elements.next());
				}
				
				process(pf, sets, 1, null);
				
			} finally {
				elements.close();
			}
			
			
			pf.done();
		}
	};
	
	private void process(PFlow pf, LinkedList<Relationship> sets, int pos, Relationship[] res) {
		if (pos > sets.size()) {
			
			System.out.println(Arrays.toString(res));
			
			//XXX: hack required!
			
			Object[] obj = null;
			for (int i = res.length-1; i >= 0; i--) {
				if (obj == null)
					obj = new Object[] {LINK._, null, new Object[] {res[i]}};
				else {
					obj = new Object[] {LINK._, null, new Object[] {res[i]}, obj};
				}
			}
			pf.sendAnswer( Expression.__(new JExpression(obj)) );
			return;
		}
		
		Relationship[] rs = new Relationship[pos];
		
		if (pos > 1) {
			System.arraycopy(res, 0, rs, 0, res.length);
		}
		
		Relationship rship = sets.get( sets.size()-pos );
		
		if (rship.isType(LINK._)) {
			IndexHits<Relationship> subelements = OrderIndex.queryDown(rship.getEndNode());
			try {
				for (Relationship r : subelements) {
					rs[pos-1] = r;
					process(pf, sets, pos+1, rs);
				}
			} finally {
				subelements.close();
			}
		} else {
			rs[pos-1] = rship;
			process(pf, sets, pos+1, rs);
		}
	}
}