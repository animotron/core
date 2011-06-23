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

import static org.neo4j.graphdb.Direction.OUTGOING;

import java.util.Iterator;

import org.animotron.Executor;
import org.jetlang.channels.Subscribable;
import org.jetlang.core.DisposingExecutor;
import org.neo4j.graphdb.Relationship;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class OnQuestion implements Subscribable<PFlow> {
	
	@Override
	public void onMessage(PFlow pf) {
		
		System.out.println("OnQuestion pf = "+pf);
		int count = 0;
		Iterator<Relationship> it = pf.getOP().getEndNode().getRelationships(OUTGOING).iterator();
		while (it.hasNext()) {
			Relationship r = it.next();
			Subscribable<PFlow> onQuestion = pf.getManipulator().onQuestion(r);
			
			if (onQuestion != null) {
				PFlow nextPF = new PFlow(pf, r);
				nextPF.question.subscribe(onQuestion);
				
				nextPF.question.publish(nextPF);
				
				count++;
			}
		}
	}

	@Override
	public DisposingExecutor getQueue() {
		System.out.println("getQueue");
		return Executor.getFiber();
	}

}
