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

import javolution.util.FastList;
import org.animotron.Executor;
import org.animotron.exception.AnimoException;
import org.animotron.graph.index.Order;
import org.animotron.statement.operator.REF;
import org.jetlang.channels.Subscribable;
import org.jetlang.core.DisposingExecutor;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.index.IndexHits;

import java.util.Iterator;
import java.util.List;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class OnQuestion implements Subscribable<PFlow> {
	
	@Override
	public void onMessage(PFlow pf) {
		
        List<PFlow> list = new FastList<PFlow>();

		IndexHits<Relationship> q = Order.queryDown(pf.getOPNode());
		try {
			boolean first = true;
			
			Iterator<Relationship> it = q.iterator();
			while (it.hasNext()) {
				Relationship r = it.next();
				
				if (first) {
					first = false;
					continue;
				}
				if (r.isType(REF._)) continue;
				
				Subscribable<PFlow> onQuestion = pf.getManipulator().onQuestion(r);
				
				if (onQuestion != null) {
					PFlow nextPF;
					try {
						nextPF = new PFlow(pf, r);
					} catch (AnimoException e) {
						continue;
					}
					nextPF.questionChannel().subscribe(onQuestion);
					
					list.add(nextPF);
					
//				} else if (RelationshipTypes.REF.name().equals(r.getType().name())) {
//					//ignore REF
				} else {
					pf.sendAnswer(pf.getVector().answered(r));
				}
			}
		} finally {
			q.close();
		}
		
		if (list.isEmpty())
			pf.done();
		else
			pf.waitBeforeClosePipe(list.size());
		
		for (PFlow nextPF : list) {
			nextPF.questionChannel().publish(nextPF);
		}
	}

	@Override
	public DisposingExecutor getQueue() {
		//System.out.println("getQueue");
		return Executor.getFiber();
	}
}