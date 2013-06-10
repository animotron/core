/*
 *  Copyright (C) 2011-2013 The Animo Project
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

import org.animotron.manipulator.QCAVector;

import java.util.Iterator;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class PipeIterator implements Iterator<QCAVector> {
	
	private Pipe pipe;
	private QCAVector next;
	
	public PipeIterator(Pipe pipe) {
		this.pipe = pipe;
		
		next = pipe.take();
	}

	@Override
	public boolean hasNext() {
		return next != null;
	}

	@Override
	public QCAVector next() {
		QCAVector obj = next;
		next = pipe.take();
		return obj;
	}

	@Override
	public void remove() {
	}
}