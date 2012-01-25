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

import java.util.Collections;
import java.util.List;

import javolution.util.FastMap;
import javolution.util.FastTable;

import org.neo4j.graphdb.Relationship;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class Profiler implements Controller {
	
	FastMap<Relationship, Profilling> sts = new FastMap<Relationship, Profilling>();
	
	long ts = 0;
	long runningTime = 0;

	@Override
	public void start() {
		ts = System.currentTimeMillis();
	}
	
	@Override
	public void end() {
		runningTime = System.currentTimeMillis() - ts;
		
		debug();
	}
	
	@Override
	public void startStatement(Relationship op) {
		Profilling pr = sts.get(op);
		if (pr == null) {
			pr = new Profilling(op);
			sts.put(op, pr);
		}
		pr.start(op);
	}

	@Override
	public void endStatement(Relationship op) {
		Profilling pr = sts.get(op);
		if (pr == null) {
			System.out.println("Profilling ERROR 1");
			return;
		}
		pr.end(op);
	}
	
	public void debug() {
		List<Profilling> prs = new FastTable<Profilling>(sts.values());
		Collections.sort(prs);
		
		for (int i = 0, n = prs.size(); i < n; i++) {
			prs.get(i).debug();
		}
		System.out.println("Total time: "+runningTime);
	}

	class Profilling implements Comparable<Profilling> {
		Relationship op;
		
		FastMap<Relationship, Long> startTS = new FastMap<Relationship, Long>();
		
		int numberOfCalls = 0;
		long timeInCalls = 0;

		public Profilling(Relationship op) {
			this.op = op;
		}
		
		public void start(Relationship op) {
			if (startTS.containsKey(op))
				System.out.println("Profilling ERROR 2");
			
			startTS.put(op, System.currentTimeMillis());
		}
		
		public void end(Relationship op) {
			Long ts = startTS.get(op);
			if (ts == null) {
				System.out.println("Profilling ERROR 3");
				return;
			}
			
			numberOfCalls++;
			timeInCalls += System.currentTimeMillis() - ts;
			
			startTS.remove(op);
		}

		@Override
		public int compareTo(Profilling o) {
			return ((Long)timeInCalls).compareTo(o.timeInCalls);
		}
		
		public void debug() {
			System.out.println("OP "+op+" ["+op.getType()+"] "+numberOfCalls+" "+timeInCalls);
		}
	}
}