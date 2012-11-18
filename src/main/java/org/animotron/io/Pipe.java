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

import javolution.lang.Reusable;
import org.animotron.manipulator.QCAVector;

import java.io.IOException;
import java.util.concurrent.SynchronousQueue;

//import javolution.context.ObjectFactory;
//import javolution.util.FastList;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class Pipe extends SynchronousQueue<QCAVector> implements Reusable {
	
	private final static QCAVector EOP = new QCAVector();

	private static final long serialVersionUID = -4081186589534725579L;
	
//    private static final ObjectFactory<Pipe> FACTORY = new ObjectFactory<Pipe>() {
//
//        public Pipe create() {
//            return new Pipe();
//        }
//    };

    public static Pipe newInstance() {
    	return new Pipe();
        //return FACTORY.object();
    }

    public static void recycle(Pipe instance) {
        //FACTORY.recycle(instance);
    }
    
	public void write(QCAVector o) throws IOException {
		try {
			put(o);
		} catch (InterruptedException e) {
			throw new IOException(e);
		}
	}
	
	public void close() {
		try {
			put(EOP);
		} catch (InterruptedException e) {
			//XXX: log
			e.printStackTrace();
		}
	}
	
	@Override
	public QCAVector take() {
		try {
			QCAVector o = super.take();

			if (o == EOP) return null;
			
			return o;
		} catch (InterruptedException e) {
			return null;
		}
	}

	@Override
	public void reset() {
		clear();
	}
}
