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
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.PropertyContainer;
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
	
	public final PipedInput execute(PropertyContainer op) throws InterruptedException, IOException {
        final PipedOutput out = new PipedOutput();
        PipedInput in = out.getInputStream();
		
        Subscribable<PFlow> sub;
		if (op instanceof Node) {
			sub = onQuestion(null);
		} else {
			sub = onQuestion((Relationship)op);
		}
		
		if (sub == null) {
			out.write(op);
			out.close();
			return in;
		}
		
		PFlow pf;
		if (op instanceof Node) {
			pf = new PFlow((StatementManipulator) this, (Node)op);
		} else {
			pf = new PFlow((StatementManipulator) this, (Relationship)op);
		}
		pf.question.subscribe(sub);

		
        //answers transfer to output
        Subscribable<Relationship> onAnswer = new Subscribable<Relationship>() {
            public void onMessage(Relationship msg) {
//            	System.out.println("get answer "+msg);
            	try {
            		if (msg == null)
            			out.close();
            		else
            			out.write(msg);
				} catch (IOException e) {
					//XXX: what to do?
					e.printStackTrace();
				}
            }

			@Override
			public DisposingExecutor getQueue() {
				//System.out.println("onAnswer getQueue");
				return Executor.getFiber();
			}
        };
//        System.out.println("pf "+pf);
//        System.out.println("pf.answer.subscribe(onAnswer) "+pf.answer);
        pf.parent.answer.subscribe(onAnswer);

        //send question to evaluation
        pf.question.publish(pf);
		
		//XXX: what to do with this?
        //reset.await(5, TimeUnit.SECONDS);
		
		return in;
	}
	
	protected Subscribable<PFlow> onQuestion(final Relationship op) {
		return new OnQuestion();
	}
}