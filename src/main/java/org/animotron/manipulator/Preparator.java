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
import org.animotron.operator.Prepare;
import org.neo4j.graphdb.PropertyContainer;
import org.neo4j.graphdb.Relationship;

/**
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
class Preparator extends Executor {
	
	static Preparator _ = new Preparator();
	
	private Preparator() {}

	@Override
	public boolean canGo(Statement statement) {
		return statement instanceof Prepare;
	}

	@Override
	public void go(Statement statement, Relationship op, PipedOutputObjectStream ot, boolean isLast) throws IOException {
		((Prepare) statement).prepare(op, ot, isLast);
		ot.close();
	}
	
	@Override
	public Walker<Preparator> walk(PropertyContainer op, PipedOutputObjectStream out) {
		return new Walker<Preparator>(this, op, out);
	}

	@Override
	public boolean isPiped() {
		return false;
	}
	
}