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
package org.animotron.addon.datetime;

import org.animotron.manipulator.OnQuestion;
import org.animotron.manipulator.PFlow;
import org.animotron.statement.instruction.DetermInstruction;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;

import java.util.WeakHashMap;

/**
 * Reference on virtual 'time' object.
 * 
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 */
public class TimestampReference extends DetermInstruction {
	
	public static final TimestampReference _ = new TimestampReference();
	
	private WeakHashMap<String, TimestampNode> pool = new WeakHashMap<String, TimestampNode>(); 
	
	private TimestampReference() { super("T"); }
	
	public void eval(Relationship op, PFlow ch) {
		Node node = op.getEndNode();
		String name = (String) node.getProperty("NAME");
		
		TimestampNode instance = pool.get(name);
		
		if (instance == null) {
			instance = new TimestampNode(node.getGraphDatabase(), name);
			TimestampNode obj = pool.put(name, instance);
			if (obj != null)
				instance = obj;
		}

		//UNDERSTAND: use in-memory relations (virtual)
		//XXX: ch.up.publish(instance);
		return;
	}

	@Override
	public OnQuestion onCalcQuestion() {
		// TODO Auto-generated method stub
		return null;
	}
}
