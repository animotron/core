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
package org.animotron;

import org.jetlang.fibers.Fiber;
import org.jetlang.fibers.PoolFiberFactory;
import org.jetlang.fibers.ThreadFiber;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

/**
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
//XXX: move to org.animotron.manipulator ?
public class Executor {
	
	//private static ExecutorService exec = Executors.newCachedThreadPool();
	private static ExecutorService exec = Executors.newFixedThreadPool(200);
	private static PoolFiberFactory fact = new PoolFiberFactory(exec);
    
	public static Fiber getFiber() {
		Fiber fiber = fact.create();
//		Fiber fiber = new ThreadFiber();
		fiber.start();
		return fiber;
	}

    public static void execute (Runnable r) {
        exec.execute(r);
    }
	
	public static void shutdown() {
		//exec.shutdown();
	}
}
