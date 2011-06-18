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

import org.animotron.io.PipedInputObjectStream;
import org.animotron.io.PipedOutputObjectStream;
import org.neo4j.graphdb.PropertyContainer;
import org.neo4j.graphdb.Relationship;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * 
 */
class UnconditionalWalker extends Walker {

	public UnconditionalWalker(SimpleManipulator m, PropertyContainer op, PipedOutputObjectStream out) {
		super(m, op, out);
	}

	@Override
	protected void go(Relationship op, PipedOutputObjectStream ot, boolean isLast) throws IOException {

		SimpleManipulator m = (SimpleManipulator) getManipulator();
		
		try {
			
			PipedInputObjectStream in = null;
			PipedOutputObjectStream out = ot;

			if (m.isPiped()) {
				in = new PipedInputObjectStream();
				out = new PipedOutputObjectStream(in);
			}

			m.go(op, out, isLast);

			if (in != null) {
				for (Object n : in) {
					ot.write(n);
				}
			}
				
		} catch (IOException e) {
			e.printStackTrace();
			ot.write(e);
		}

		ot.close();
	}

}
