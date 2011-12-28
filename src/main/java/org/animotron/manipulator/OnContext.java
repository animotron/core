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
package org.animotron.manipulator;

import java.util.concurrent.CountDownLatch;

import org.jetlang.channels.Subscribable;
import org.jetlang.core.DisposingExecutor;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public abstract class OnContext implements Subscribable<QCAVector> {
	
	DisposingExecutor queue;
	CountDownLatch cd = null;
	
	public OnContext(DisposingExecutor queue) {
		this.queue = queue;		
	}
	
	public void setCountDown(int number) {
		cd = new CountDownLatch(number);
	}
	
	@Override
	public void onMessage(QCAVector vector) {
		if (vector == null && cd != null)
			cd.countDown();
	}
	

	@Override
	public DisposingExecutor getQueue() {
		return queue;
	}

	public void isDone() {
		if (cd != null)
			try {
				cd.wait();
			} catch (InterruptedException e) {
			}
	}
}
