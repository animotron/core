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
package org.animotron.addon.datetime;

import java.io.IOException;
import java.util.WeakHashMap;

import org.animotron.io.PipedOutputObjectStream;
import org.animotron.operator.AbstractOperator;
import org.animotron.operator.Evaluable;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;

/**
 * Reference on virtual 'time' object.
 * 
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 */
public class TimestampReference extends AbstractOperator implements Evaluable {
	
	public static final TimestampReference INSTANCE = new TimestampReference();
	public static TimestampReference getInstance() { return INSTANCE; }
	
	private WeakHashMap<String, TimestampNode> pool = new WeakHashMap<String, TimestampNode>(); 
	
	private TimestampReference() { super("T", "animo/time"); }
	
	@Override
	public void eval(Relationship op, PipedOutputObjectStream out, boolean isLast) throws IOException {
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
		out.write(instance);
		out.close();
		return;
	}
}
