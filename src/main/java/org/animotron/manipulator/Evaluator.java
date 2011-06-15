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

import org.animotron.Statement;
import org.animotron.io.PipedOutputObjectStream;
import org.animotron.operator.Evaluable;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
class Evaluator implements Manipulator {

	public static Evaluator _ = new Evaluator();
	
	private Evaluator() {}

	@Override
	public boolean canGo(Statement statement) {
		return statement instanceof Evaluable;
	}

	@Override
	public void go(Statement statement, Relationship op, PipedOutputObjectStream ot, boolean isLast) throws IOException {
		
		((Evaluable) statement).eval(op, ot, isLast);
		
		ot.close();
	}

	public Walker<Evaluator> walk(Node op, PipedOutputObjectStream out) {
		return new Walker<Evaluator>(this, op, null, out);
	}

	public Walker<Evaluator> walk(Relationship op, PipedOutputObjectStream out) {
		return new Walker<Evaluator>(this, null, op, out);
	}
	
	// for debug needs
	public boolean isPiped() {
		return true;
	}
}