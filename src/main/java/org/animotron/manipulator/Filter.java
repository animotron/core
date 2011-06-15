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
import org.animotron.operator.Predicate;
import org.neo4j.graphdb.Relationship;

/**
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
class Filter extends Walker {

	public Filter(Relationship op, PipedOutputObjectStream out) {
		super(null, op, out);
	}

	@Override
	protected boolean canGo(Statement statement) {
		return statement instanceof Predicate;
	}

	@Override
	protected void go(Statement statement, Relationship op, PipedOutputObjectStream ot, boolean isLast) throws IOException {
		
		((Predicate) statement).filter(op, ot, isLast);
		
		if (isPiped())
			ot.close();
	}
	
	public boolean isPiped() {
		return false;
	}
}