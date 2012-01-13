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
package org.animotron.manipulator;

import java.io.IOException;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public abstract class OnQuestion extends Subscribable<PFlow> {
	
	public OnQuestion() {
		super(null);
	}

	@Override
	public final void onMessage(final PFlow pf) {
		
		try {
		
			act(pf);
		
		} catch (Exception e) {
			pf.sendException(e);
		
		} finally {
			pf.done();
		}
		
//		System.out.println("Common OnQuestion in use");
//		
//		OnContext onAnswer = new OnContext(Executor.getFiber()) {
//            public void onMessage(QCAVector context) {
//            	super.onMessage(context);
//            	
//            	pf.answerChannel().publish(context);
//            }
//        };
//
//		Evaluator.sendQuestion(onAnswer, pf.getVector(), pf.getVector().getUnrelaxedClosest().getEndNode());
	}
	
	public abstract void act(final PFlow pf) throws IOException;

}
