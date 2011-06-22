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
package org.animotron.instruction.string;

import static org.animotron.graph.AnimoGraph.getORDER;

import org.animotron.instruction.AbstractInstruction;
import org.animotron.manipulator.PFlow;
import org.animotron.operator.Evaluable;
import org.animotron.serializer.StringResultSerializer;
import org.neo4j.graphdb.Relationship;

/**
 * String instruction 'after-last'.
 * 
 * Return last chars from input string after last-found defined pattern.
 * 
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 */
public class AfterLast extends AbstractInstruction implements Evaluable {
	
	public static final AfterLast _ = new AfterLast();
	
	private AfterLast() { super("after-last", STRING._); }
	
	@Override
	public void eval(Relationship op, PFlow ch, boolean isLast) {
		
		//UNDERSTAND: if we have more that 2 params, what to do?
		
		Relationship[] params = getORDER().first(2, op.getEndNode());
		
		//pattern
		StringResultSerializer szer = new StringResultSerializer();
		szer.serialize(params[0]);
		String pattern = szer.getString();

		szer.serialize(params[1]);
		String source = szer.getString();
		
		int index = source.lastIndexOf(pattern);
		String result = source.substring(index);
		
		System.out.println(result);
	}
}
