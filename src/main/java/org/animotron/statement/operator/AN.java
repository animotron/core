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
package org.animotron.statement.operator;

import org.animotron.io.PipedInput;
import org.animotron.manipulator.OnQuestion;
import org.animotron.manipulator.PFlow;
import org.animotron.manipulator.QCAVector;

/**
 * Operation 'AN'. Direct reference to 'the' instance.
 * 
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 */
public class AN extends Operator implements Reference, Evaluable, Shift {
	
	public static final AN _ = new AN();
	
	private static boolean debug = false;
	
	private AN() { super("an"); }
	
    @Override
	public OnQuestion onCalcQuestion() {
		return question;
	}
	
	private OnQuestion question = new OnQuestion() {

		@Override
		public void onMessage(final PFlow pf) {
			
			byte[] hash = pf.getOpHash();

			if (debug) System.out.println("AN "+Thread.currentThread());
			//System.out.println("AN "+pf.getVector());
			//pf.sendAnswer(new QCAVector(op,op));
			
			if (!Utils.results(pf, hash)) {
				for (QCAVector r : getREFs(pf, pf.getVector())) {
					pf.sendAnswer(r);
				}
			}
			pf.done();
		}
	};
	
	public static PipedInput<QCAVector> getREFs(final PFlow pf, final QCAVector vector) {
		return Utils.getByREF(pf, vector);
	}
}
