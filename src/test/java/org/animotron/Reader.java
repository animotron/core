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
import java.io.InputStream;
import java.io.PipedInputStream;
import java.io.PipedOutputStream;

import org.animotron.manipulator.Channels;
import org.jetlang.core.Callback;
import org.jetlang.fibers.Fiber;
import org.neo4j.graphdb.Relationship;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class Reader {
	
	public static InputStream read(Relationship position) throws IOException {
		
		Fiber fiber = Executor.getFiber();
		
		PipedInputStream in = new PipedInputStream();
		final PipedOutputStream out = new PipedOutputStream(in);
		
		final Channels ch = new Channels(); 

		Callback<Relationship> onReceive = new Callback<Relationship>() {

			@Override
			public void onMessage(Relationship r) {
				String typeName = r.getType().name();

				if (r.getType().name().startsWith("the:")) {
					try {
						out.write(("<"+typeName+">").getBytes());
						out.write(("</"+typeName+">").getBytes());
					} catch (IOException e) {
						
					}
				}
			}
		};
		
		ch.up.subscribe(fiber, onReceive);

		return in;
	}
}
