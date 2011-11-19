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
import org.neo4j.graphdb.Relationship;

import static org.animotron.graph.RelationshipTypes.REF;

/**
 * Operation 'AN'. Direct reference to 'the' instance.
 * 
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 */
public class AN extends Operator implements Reference, Evaluable {
	
	public static final AN _ = new AN();
	
	private AN() { super("an"); }
	
    @Override
	public OnQuestion onCalcQuestion() {
		return question;
	}
	
	private OnQuestion question = new OnQuestion() {

		@Override
		public void onMessage(final PFlow pf) {
			
			Relationship op = pf.getOP();
			
			byte[] hash = pf.getOpHash();

			//System.out.println("AN "+op+" "+pf.getOpHash()+" ");
			
			if (!Utils.results(pf, hash)) {

				for (ACQVector r : getREFs(pf, op)) {

					pf.sendAnswer(r.getAnswer(), REF, hash);
				}
			}
			pf.done();
		}
	};
	
	public static PipedInput<ACQVector> getREFs(PFlow pf, Relationship op) {
		return Utils.getByREF(pf, op);
	}
}