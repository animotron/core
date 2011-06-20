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
package org.animotron.walker;

import java.io.IOException;

import org.animotron.Catcher;
import org.animotron.io.PipedOutput;
import org.animotron.manipulator.SimpleManipulator;
import org.animotron.marker.Marker;
import org.neo4j.graphdb.PropertyContainer;
import org.neo4j.graphdb.Relationship;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * 
 */
public class UnconditionalWalker extends Walker {

	public UnconditionalWalker(SimpleManipulator m, PropertyContainer op, PipedOutput out, Marker marker) {
		super(m, op, out, marker);
	}

	@Override
	protected void go(Relationship op, PipedOutput ot, Catcher catcher, boolean isLast) throws IOException {

		SimpleManipulator m = (SimpleManipulator) getManipulator();
		
		try {
			m.go(op, ot, catcher, isLast);
				
		} catch (IOException e) {
			e.printStackTrace();
			ot.write(e);
		}

	}

}
