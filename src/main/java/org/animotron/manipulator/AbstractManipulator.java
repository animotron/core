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

import org.animotron.Executor;
import org.animotron.io.PipedInputObjectStream;
import org.animotron.io.PipedOutputObjectStream;
import org.animotron.walker.Walker;
import org.neo4j.graphdb.PropertyContainer;

/**
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public abstract class AbstractManipulator implements Manipulator {
	
	@Override
	public final PipedInputObjectStream markExecute(PropertyContainer op) throws IOException {
		return Executor.markExecute(this, op);
	}

	@Override
	public final void markExecute(PropertyContainer op, PipedOutputObjectStream out) {
		Executor.markExecute(this, op, out);
	}
	
	@Override
	public final PipedInputObjectStream execute(PropertyContainer op) throws IOException {
		return Executor.execute(this, op);
	}

	@Override
	public final void execute(PropertyContainer op, PipedOutputObjectStream out) {
		Executor.execute(this, op, out);
	}

	@Override
	public boolean isPiped() {
		return true;
	}
	
	@Override
	public void shutdown() {
		
	}
	
	@Override
	public Walker walk(PropertyContainer op, PipedOutputObjectStream out) {
		return walk(op, out, null);
	}
	
}
