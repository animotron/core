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
import java.util.concurrent.Executors;

import org.animotron.io.PipedInputObjectStream;
import org.animotron.io.PipedOutputObjectStream;
import org.neo4j.graphdb.PropertyContainer;

/**
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public abstract class Executor implements Manipulator {
	
	private static int THREADS_NUMBER = 100;
	private static java.util.concurrent.Executor exec = Executors.newFixedThreadPool(THREADS_NUMBER);
	
	protected void execute(Runnable command){
		exec.execute(command);
	}
	
	public PipedInputObjectStream execute(PropertyContainer op) throws IOException {
		PipedInputObjectStream in = new PipedInputObjectStream();
		execute(walk(op, new PipedOutputObjectStream(in)));
		return in;
	}

	public void execute(PropertyContainer op, PipedOutputObjectStream out) {
		execute(walk(op, out));
	}
	

}
