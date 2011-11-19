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

import org.animotron.io.PipedInput;
import org.animotron.manipulator.ACQVector;
import org.animotron.manipulator.OnQuestion;
import org.animotron.manipulator.PFlow;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;

import java.util.List;

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
			
			System.out.println("THIS "+op+" ");
			PipedInput<ACQVector> thes = AN.getREFs(pf, op);
			//System.out.println(Arrays.toString(thes.toArray()));

			//System.out.println("AN "+op+" "+pf.getOpHash()+" ");
			
			if (!Utils.results(pf)) {
				
				for (ACQVector r : pf.getPFlowPath()) {
					System.out.print(""+r+" ");
					System.out.print(""+r.getQuestion().getStartNode()+" -> ");
					System.out.print(""+r.getQuestion()+" ");
					
//					for (Relationship rr : Utils.td_eval_IS.traverse(r.getEndNode()).relationships()) {
//						System.out.print("["+rr+" ");
//						System.out.print(""+rr.getStartNode()+" -> ");
//						System.out.print(""+rr.getEndNode()+"] ");
//					}
					System.out.println("");
					
					//pf.sendAnswer(createResult(pf, op, node, r, REF, pf.getOpHash()), op);
				}
			}
			pf.done();
		}
	};
}