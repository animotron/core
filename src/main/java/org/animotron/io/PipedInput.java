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

import org.animotron.expression.Expression;
import org.animotron.utils.Utils;

import java.io.IOException;
import java.util.Iterator;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class PipedInput<T> implements Cloneable, Iterable<T>, Iterator<T> {
    
	protected boolean closedByWriter = false;
	protected volatile boolean closedByReader = false;
	protected boolean connected = false;
	
    private Thread readSide;
    private Thread writeSide;

    protected int in = -1;
    protected int out = 0;
    
    private static final int DEFAULT_PIPE_SIZE = 1024;
    
    protected Object buffer[] = new Object[DEFAULT_PIPE_SIZE];
    
    private synchronized Object read()  throws IOException {
        if (!connected) {
            throw new IOException("Pipe not connected");
            
        } else if (closedByReader) {
        	throw new IOException("Pipe closed");
        	
        } else if (writeSide != null && !writeSide.isAlive() && !closedByWriter && (in < 0)) {
            throw new IOException("Write end dead");
        }

        readSide = Thread.currentThread();
        int trials = 2;
        while (in < 0) {
        	if (closedByWriter) {
        		/* closed by writer, return EOF */
        		return null;//-1;
        	}
        	if ((writeSide != null) && (!writeSide.isAlive()) && (--trials < 0)) {
        		throw new IOException("Pipe broken");
        	}
            
        	/* might be a writer waiting */
        	notifyAll();
        	try {
        		wait(1000);
        	} catch (InterruptedException ex) {
        		throw new java.io.InterruptedIOException();
        	}
        }
        Object ret = buffer[out++];
        if (out >= buffer.length) {
        	out = 0;
        }
        if (in == out) {
            /* now empty */
        	in = -1;
        }

        return ret;
    }
    
    public void close()  throws IOException {
    	closedByReader = true;
        synchronized (this) {
            in = -1;
        }
    }
    
	public void receive(Object obj) throws IOException {
        checkStateForReceive();
        
        writeSide = Thread.currentThread();
        if (in == out)
            awaitSpace();
        
        if (in < 0) {
        	in = 0;
        	out = 0;
        }
        
        buffer[in++] = obj;
        if (in >= buffer.length) {
        	in = 0;
        }
	}
	
    protected synchronized void receivedLast() {
    	closedByWriter = true;
    	notifyAll();
    }
    
    private void checkStateForReceive() throws IOException {
        if (!connected) {
            throw new IOException("Pipe not connected");
            
        } else if (closedByWriter) {
        	throw new IOException("Pipe closed by writer "+Utils.shortID(this));
        	
        } else if (closedByReader) {
        	throw new IOException("Pipe closed by reader");

        } else if (readSide != null && !readSide.isAlive()) {
            throw new IOException("Read end dead");
            
        }
    }

    private void awaitSpace() throws IOException {
    	while (in == out) {
    	    checkStateForReceive();

    	    /* full: kick any waiting readers */
    	    notifyAll();
    	    try {
    	        wait(1000);
    	    } catch (InterruptedException ex) {
    		throw new java.io.InterruptedIOException();
    	    }
    	}
    }

	@Override
	public Iterator<T> iterator() {
		return this;
	}

    private T current = null;
    private boolean first = true;

    @Override
    public boolean hasNext() {
        if (first) {
            next();
            first = false;
        }
        return current != null;
    }

    @Override
    public T next() {
        T next = current;
        current = step();
        return next;
    }

    private T step() {
        try {
            T o = (T) read();
            if (o instanceof Expression)
                return null;
            return o;
        } catch (ClassCastException e) {
            return step();
        } catch (IOException e) {
            return null;
        }
    }

    @Override
    public void remove() {
    }

}