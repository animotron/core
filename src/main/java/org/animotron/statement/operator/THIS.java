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
package org.animotron.statement.operator;

import org.animotron.manipulator.QCAVector;
import org.animotron.manipulator.OnQuestion;
import org.animotron.manipulator.PFlow;
import org.animotron.statement.relation.HAVE;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;

import java.util.Set;

import javolution.util.FastSet;

/**
 * Operation 'THIS'. Reference to the closest instance in PFlow.
 * 
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 */
public class THIS extends Operator implements Reference, Evaluable {

	public static final THIS _ = new THIS();

	private THIS() { super("this"); }
	
    @Override
	public OnQuestion onCalcQuestion() {
		return question;
	}
	
	private OnQuestion question = new OnQuestion() {

		@Override
		public void onMessage(final PFlow pf) {
			
			Relationship op = pf.getOP();
			
			final Set<Node> thes = new FastSet<Node>(); 
			for (QCAVector theNode : AN.getREFs(pf, op)) {
				thes.add(theNode.getAnswer().getEndNode());
			}

			Utils.debug(THIS._, op, thes);
			
			if (!Utils.results(pf)) {
				
				for (QCAVector r : pf.getPFlowPath()) {
					//System.out.println(r);
					QCAVector[] cs = r.getContext();
					if (cs != null)
						for (QCAVector c : cs) {
							Relationship toCheck = c.getQuestion();
							if (toCheck.isType(HAVE._)) {
								if (thes.contains( Utils.getByREF(toCheck).getEndNode() )) {
									pf.sendAnswer(new QCAVector(op, c.getUnrelaxedAnswer(), c.getContext()));
									pf.done();
									return;
								}
							}
						}
				}
			}
			pf.done();
		}
	};
}