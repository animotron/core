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

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * 
 */
public class PipedOutput<T> implements Cloneable {

	private PipedInput<T> connection;
	
	private boolean isEmpty = true;

	public PipedOutput() {
		try {
			connect(new PipedInput<T>());
		} catch (IOException e) {}
	}

	public PipedOutput(PipedInput<T> inStream) throws IOException {
		connect(inStream);
	}
	
	public PipedInput<T> getInputStream() {
		return connection;
	}

	protected synchronized void connect(PipedInput<T> inStream) throws IOException {
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
	
    public void write(T obj) throws IOException {
    	//System.out.println("Write to pipe "+Utils.shortID(this)+" "+Utils.shortID(connection));
        
    	if (connection == null) throw new IOException("Pipe not connected");

        connection.receive(obj);
        isEmpty = false;
    }
    
    public void close()  throws IOException {
    	//System.out.println("Closing pipe "+Utils.shortID(this)+" "+Utils.shortID(connection));
    	if (connection != null) {
    		connection.receivedLast();
    	}
    }

	public boolean isEmpty() {
		return isEmpty;
	}
}
