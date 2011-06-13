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
package org.animotron.interpreter;

import java.io.IOException;

import org.animotron.Statement;
import org.animotron.io.PipedOutputObjectStream;
import org.animotron.operator.Evaluable;
import org.neo4j.graphdb.Relationship;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
class Evaluator extends Walker {

	public Evaluator(Relationship op, PipedOutputObjectStream out) {
		super(op, out);
		// TODO Auto-generated constructor stub
	}

	@Override
	protected boolean isInstance(Statement statement) {
		return statement instanceof Evaluable;
	}

	@Override
	protected void go(Statement statement, Relationship op,
			PipedOutputObjectStream ot, boolean isLast) throws IOException {
		statement.eval(op, ot, isLast);
	}
	
}