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
package org.animotron;

import java.io.IOException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import org.animotron.manipulator.Channels;
import org.animotron.manipulator.Manipulator;
import org.neo4j.graphdb.PropertyContainer;

/**
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class Executor {
	
	private static int THREADS_NUMBER = 100;
	private static ExecutorService exec = Executors.newFixedThreadPool(THREADS_NUMBER);
	
	public static void execute(Runnable command){
		exec.execute(command);
	}
	
	public static Channels execute(Manipulator m, PropertyContainer op) throws IOException {
		Channels ch = new Channels();
		execute(m.walk(op, ch));
		return ch;
	}

	public static Channels markExecute(Manipulator m, PropertyContainer op) throws IOException {
		Channels ch = new Channels();
		execute(m.markWalk(op, ch));
		return ch;
	}

	public static void execute(Manipulator m, PropertyContainer op, Channels ch) {
		execute(m.walk(op, ch));
	}
	
	public static void markExecute(Manipulator m, PropertyContainer op, Channels ch) {
		execute(m.markWalk(op, ch));
	}
	
	public static void shutdown() {
		exec.shutdown();
	}
}
