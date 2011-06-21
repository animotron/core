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
package org.animotron.operator.compare;

import java.io.IOException;

import org.animotron.io.PipedOutput;
import org.animotron.manipulator.Channels;
import org.animotron.operator.AbstarctOperator;
import org.animotron.operator.Predicate;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;

/**
 * Compare operator 'NE'.
 * 
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 */
public class NE extends AbstarctOperator implements Predicate {
	
	public static final NE _ = new NE();
	
	private NE() { super("ne", "animo/compare/ne"); }

	@Override
	public void filter(Relationship op, Channels ch, boolean isLast) throws IOException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public boolean filter(Relationship op, Node ref) {
		// TODO Auto-generated method stub
		return false;
	}
	
}
