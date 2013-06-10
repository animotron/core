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
package org.animotron;

import org.jetlang.fibers.Fiber;
import org.jetlang.fibers.PoolFiberFactory;

import java.util.concurrent.SynchronousQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

/**
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
//XXX: move to org.animotron.manipulator ?
public class Executor {
	
	private static int N_THREADS = 400;
	//private static int CAPACITY = 20;
	
	private static ThreadPoolExecutor exec;
	private static PoolFiberFactory fact;
	
	public static void init() {
		//if (exec != null) return;
		//exec = Executors.newFixedThreadPool(400);
		
		exec = new ThreadPoolExecutor(N_THREADS, N_THREADS,
	        3L, TimeUnit.MINUTES,
	        new SynchronousQueue<Runnable>());//new LinkedBlockingQueue<Runnable>(CAPACITY)
		
		exec.setRejectedExecutionHandler(
				new ThreadPoolExecutor.CallerRunsPolicy());
	
		fact = new PoolFiberFactory(exec);
	}

	public static Fiber getFiber() {
		//System.out.println("Active threads "+exec.getActiveCount());
		//(new IOException()).printStackTrace();
		
		Fiber fiber = fact.create();
		fiber.start();
		return fiber;
	}

    public static void execute (Runnable r) {
    	exec.submit(r);
    }
	
	public static void shutdown() {
		exec.shutdown();
		try {
			int i = 2;
			while (!exec.isTerminated()) {
				System.out.println("Active threads "+exec.getActiveCount());
				
				Thread.sleep(100);
				i--;
				
				if (i == 0)
					System.out.println(exec.shutdownNow());
			}
		
		} catch (InterruptedException e) {
			e.printStackTrace();
		}
	}
}
