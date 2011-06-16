package org.animotron.manipulator;

import java.util.concurrent.Executors;

public class Executor {
	
	private static int THREADS_NUMBER = 100;
	private static java.util.concurrent.Executor exec = Executors.newFixedThreadPool(THREADS_NUMBER);
	
	public static void execute(Runnable command){
		exec.execute(command);
	}
	
}
