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
package org.animotron.manipulator;

import java.io.IOException;
import java.util.concurrent.CountDownLatch;

import org.animotron.io.Pipe;
import org.jetlang.fibers.Fiber;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public abstract class OnContext extends Subscribable<QCAVector> {
	
	protected CountDownLatch cd = null;
	
	public OnContext() {
		super(null);
	}

	public OnContext(Fiber fiber) {
		super(fiber);		
	}
	
	public void setCountDown(int number) {
		cd = new CountDownLatch(number);
	}
	
	@Override
	public void onMessage(QCAVector vector) {
		countDown(vector);
	}

	public void onMessage(QCAVector vector, Pipe pipe) {
		countDown(vector);
		
		if (cd.getCount() == 0)
			pipe.close();
		else
			try {
				pipe.write(vector);
			} catch (IOException e) {
				//XXX: log
				e.printStackTrace();
			}
	}

	public void countDown(QCAVector vector) {
		if (vector == null && cd != null) { 
			cd.countDown();
			//if (cd.getCount() == 0)
				//fiber.dispose();
		}
	}
	
	public void isDone() throws IOException {
		if (cd != null)
			try {
				cd.await();
			} catch (InterruptedException e) {
				throw new IOException(e);
			}
	}
}
