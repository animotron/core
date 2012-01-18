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

import org.animotron.Executor;
import org.jetlang.core.DisposingExecutor;
import org.jetlang.fibers.Fiber;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public abstract class Subscribable<T> implements org.jetlang.channels.Subscribable<T> {

	Fiber fiber;
	
	protected Subscribable() {
		fiber = null;
	}

	public Subscribable(Fiber fiber) {
		if (fiber != null)
			this.fiber = fiber;
		else
			this.fiber = Executor.getFiber();
	}

	@Override
	public DisposingExecutor getQueue() {
		return fiber;
	}

	public Fiber getFiber() {
		return fiber;
	}
}
