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
public class PipedOutputObjectStream implements Cloneable {

	private PipedInputObjectStream connection;

	public PipedOutputObjectStream(PipedInputObjectStream inStream) throws IOException {
		connect(inStream);
	}

	protected synchronized void connect(PipedInputObjectStream inStream) throws IOException {
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

        connection.receive(obj);
    }
    
    public void close()  throws IOException {
    	if (connection != null) {
    		connection.receivedLast();
    	}
    }
}
