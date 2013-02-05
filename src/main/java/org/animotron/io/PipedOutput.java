/*
 *  Copyright (C) 2011-2012 The Animo Project
 *  http://animotron.org
 *  	
 *  This file is part of Animotron.
 *  
 *  Animotron is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU Affero General Public License as 
 *  published by the Free Software Foundation, either version 3 of 
 *  the License, or (at your option) any later version.
 *  
 *  Animotron is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Affero General Public License for more details.
 *  
 *  You should have received a copy of 
 *  the GNU Affero General Public License along with Animotron.  
 *  If not, see <http://www.gnu.org/licenses/>.
 */
package org.animotron.io;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;

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
		
		debug();
	}
	
	public void debug() {
		//debug(false);
	}

	public void debug(boolean stack) {
		System.out.println("PipedOutput "+hash2string(this)+" -> "+hash2string(connection));
		if (stack) System.out.println(stack2string(new IOException()).substring(0, 150));
	}
	
	private static String hash2string(Object e) {
		String s = e.toString();
		return s.substring(s.indexOf("@")+1);
	}
	
	private static String stack2string(Throwable t) {
		try {
			StringWriter sw = new StringWriter();
			PrintWriter pw = new PrintWriter(sw);
			t.printStackTrace(pw);
			return sw.toString();
		} catch(Throwable t2) {
			t2.printStackTrace();
		}
		return "";
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
    	//System.out.print("closing ");
		//debug();

    	if (connection != null) {
    		connection.receivedLast();
    	}
    }

	public boolean isEmpty() {
		return isEmpty;
	}
}
