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

import java.io.IOException;

import org.animotron.Executor;
import org.animotron.io.PipedInput;
import org.animotron.io.PipedOutput;
import org.animotron.marker.Marker;
import org.jetlang.channels.Subscribable;
import org.jetlang.core.DisposingExecutor;
import org.neo4j.graphdb.Relationship;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public abstract class Manipulator {

//	protected abstract void prepare(Relationship op, PFlow ch);

	public Marker marker(){
		return null;
	}
	
	public final PipedInput execute(Relationship op) throws InterruptedException {
		PFlow pf = new PFlow((StatementManipulator) this);
		
        final PipedOutput out = new PipedOutput();
        PipedInput in = out.getInputStream();
		
//		//finish watcher
//		final CountDownLatch reset = new CountDownLatch();
//        Callback<Void> onStop = new Callback<Void>() {
//            public void onMessage(Void msg) {
//            	System.out.println("get stop");
//
//            	reset.countDown();
//                
//                if (reset.getCount() == 0)
//                	try {
//                		System.out.println("closing ...");
//                		//fiber.dispose();
//						out.close();
//					} catch (IOException e) {
//						//XXX: what to do?
//						e.printStackTrace();
//					}
//            }
//        };
//        pf.stop.subscribe(fiber, onStop);

        //answers transfer to output
        Subscribable<Relationship> onAnswer = new Subscribable<Relationship>() {
            public void onMessage(Relationship msg) {
            	System.out.println("get answer "+msg);
            	try {
					out.write(msg);
				} catch (IOException e) {
					//XXX: what to do?
					e.printStackTrace();
				}
            }

			@Override
			public DisposingExecutor getQueue() {
				System.out.println("onAnswer getQueue");
				return Executor.getFiber();
			}
        };
        pf.answer.subscribe(onAnswer);

        pf.question.subscribe(new OnQuestion());
        
        //send question to evaluation
        pf.question.publish(new PFlow(pf, op));
		
		//XXX: what to do with this?
        //reset.await(5, TimeUnit.SECONDS);
		
		return in;
	}
}