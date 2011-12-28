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
package org.animotron.manipulator;

import org.animotron.Executor;
import org.jetlang.channels.Subscribable;
import org.jetlang.core.DisposingExecutor;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class OnQuestion implements Subscribable<PFlow> {
	
	@Override
	public void onMessage(final PFlow pf) {
		
		System.out.println("Common OnQuestion in use");
		
		OnContext onAnswer = new OnContext(Executor.getFiber()) {
            public void onMessage(QCAVector context) {
            	pf.answerChannel().publish(context);
            }
        };

		Evaluator.sendQuestion(onAnswer, pf.getVector(), pf.getVector().getUnrelaxedClosest().getEndNode());
		
//        List<PFlow> list = new FastList<PFlow>();
//
//		IndexHits<Relationship> q = Order.context(pf.getOPNode());
//		try {
//			Iterator<Relationship> it = q.iterator();
//			while (it.hasNext()) {
//				Relationship r = it.next();
//				
//				Subscribable<PFlow> onQuestion = Evaluator._.onQuestion(r);
//				
//				if (onQuestion != null) {
//					PFlow nextPF = new PFlow(pf, pf.getVector().question2(r));
//					nextPF.questionChannel().subscribe(onQuestion);
//					
//					list.add(nextPF);
//					
//				} else {
//					pf.sendAnswer(pf.getVector().answered(r));
//				}
//			}
//		} finally {
//			q.close();
//		}
//		
//		if (list.isEmpty())
//			pf.done();
//		else
//			pf.waitBeforeClosePipe(list.size());
//		
//		for (PFlow nextPF : list) {
//			nextPF.questionChannel().publish(nextPF);
//		}
	}

	@Override
	public DisposingExecutor getQueue() {
		//System.out.println("getQueue");
		return Executor.getFiber();
	}
}