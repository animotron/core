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

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import org.animotron.annotation.Namespace;
import org.animotron.operator.Operator;
import org.clapper.util.classutil.AbstractClassFilter;
import org.clapper.util.classutil.AndClassFilter;
import org.clapper.util.classutil.ClassFilter;
import org.clapper.util.classutil.ClassFinder;
import org.clapper.util.classutil.ClassInfo;
import org.clapper.util.classutil.InterfaceOnlyClassFilter;
import org.clapper.util.classutil.NotClassFilter;
import org.clapper.util.classutil.SubclassClassFilter;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class Operators {
	
	private static boolean ready = false;
	
	protected static void scan() {
		Thread scanner = new Thread(new Runnable() {
			@Override
			public void run() {
				//create class finder
				ClassFinder finder = new ClassFinder();
				
				//add class paths as seaching place
				finder.addClassPath();
				
				//create filter for 'Operator' implementations
				ClassFilter filter = 
					new AndClassFilter(
						// Must implement the Operator interface
						new SubclassClassFilter (Operator.class),

						// Must not be an interface
						new NotClassFilter (new InterfaceOnlyClassFilter()),

						// Must not be abstract
						new NotClassFilter (new AbstractClassFilter()));
				
				Collection<ClassInfo> foundClasses = new ArrayList<ClassInfo>();
				
				//searching
				finder.findClasses(foundClasses, filter);

				//scan classes
				for (ClassInfo classInfo : foundClasses) {
		            Class<? extends Operator> clazz;
					try {
						clazz = (Class<? extends Operator>) Class.forName( classInfo.getClassName() );
			            if (clazz.isAnnotationPresent(Namespace.class) ) {
			            	Namespace ns = clazz.getAnnotation(Namespace.class);

			            	uriToClass.put(ns.uri(), clazz);
			            }
					} catch (ClassNotFoundException e) {
						//should not happen
						//TODO: log
						e.printStackTrace();
					}
		            
				}
				
				Operators.ready = true;
			}
		});
		
		scanner.start();
	}
	
	private static Map<String, Class<? extends Operator >> uriToClass = 
		new HashMap<String, Class<? extends Operator>>();
	
	public static Class<? extends Operator> get(String uri) {
		ready();
		
		return uriToClass.get(uri);
	}

	public static void ready() {
		while (!ready) {
			try {
				Thread.sleep(500);
			} catch (InterruptedException e) {
				return;
			}
		}
	}
}
