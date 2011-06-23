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
package org.animotron.io;

import java.io.IOException;
import java.util.List;

import javolution.util.FastList;

import org.animotron.operator.Predicate;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * 
 */
public class PipedOutput implements Cloneable {

	private PipedInput connection;

	public PipedOutput() {
		try {
			connect(new PipedInput());
		} catch (IOException e) {}
	}

	public PipedOutput(PipedInput inStream) throws IOException {
		connect(inStream);
	}
	
	public PipedInput getInputStream() {
		return connection;
	}

	protected synchronized void connect(PipedInput inStream) throws IOException {
		if (inStream == null) {
			throw new NullPointerException();
		
		} else if (connection != null || inStream.connected) {
			throw new IOException("Already connected");
		
		}
		
		connection = inStream;
		
		inStream.in = -1;
		inStream.out = 0;
		inStream.connected = true;
	}
	
    public void write(Object obj) throws IOException {
        
    	if (connection == null) throw new IOException("Pipe not connected");

    	//System.out.println("pipe write "+obj);
    	
        connection.receive(obj);
    }
    
    public void close()  throws IOException {
    	System.out.println("out-pipe close");
    	if (connection != null) {
    		connection.receivedLast();
    	}
    }
    
    List<Object[]> filters = new FastList<Object[]>();

	public boolean filter(Node ref) throws IOException {
		for (Object[] filterTube : filters) {
			if (!((Predicate)filterTube[1]).
					filter(
						(Relationship)filterTube[0], 
						ref)
					) 
			{
				return false;
			}
		}
		return true;
	}

	public void subscribeFilter(Relationship op, Predicate filter) {
		filters.add(new Object[] {op, filter});
	}
}
