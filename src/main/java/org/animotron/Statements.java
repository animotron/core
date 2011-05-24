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

import java.lang.reflect.Method;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import javolution.util.FastList;
import javolution.util.FastMap;

import org.animotron.annotation.Namespace;
import org.animotron.instruction.Instruction;
import org.animotron.instruction.InstructionContainer;
import org.clapper.util.classutil.*;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class Statements {
	
	private static boolean ready = false;
	
	protected static void scan() {
		Thread scanner = new Thread(new Runnable() {
			@Override
			public void run() {
				//create class finder
				ClassFinder finder = new ClassFinder();
				
				//add class paths as seaching place
				finder.addClassPath();
				
				//create filter for 'Statement' implementations
				ClassFilter filter = 
					new AndClassFilter(
						// Must implement the Statement interface
						new SubclassClassFilter (Statement.class),

						// Must not be an interface
						new NotClassFilter (new InterfaceOnlyClassFilter()),

						// Must not be abstract
						new NotClassFilter (new AbstractClassFilter()));
				
				Collection<ClassInfo> foundClasses = new FastList<ClassInfo>();
				
				//searching
				finder.findClasses(foundClasses, filter);

				//instruction will be added to container
            	Map<String, List<Instruction>> instructions = 
            		new FastMap<String, List<Instruction>>();

				//scan classes
				for (ClassInfo classInfo : foundClasses) {
		            Class<? extends Statement> clazz;
					try {
						clazz = (Class<? extends Statement>) Class.forName( classInfo.getClassName() );
			            if (clazz.isAnnotationPresent(Namespace.class) ) {
			            	Namespace ns = clazz.getAnnotation(Namespace.class);
			            	
							try {
								Method method = clazz.getMethod("getInstance", null);
				            	Object obj = method.invoke(clazz, null);

				            	if (obj instanceof Instruction) {
				            		List<Instruction> list = instructions.get(ns.uri());
				            		if (list == null) {
				            			list = new FastList<Instruction>();
				            			instructions.put(ns.uri(), list);
				            		}
				            		list.add((Instruction) obj);
									
								} else if (obj instanceof Statement) {
									statements.put(ns.uri(), (Statement) obj);
								} else {
									//TODO: log?
								}
							} catch (Exception e) {
								//TODO: log
								e.printStackTrace();
							}
			            }
					} catch (ClassNotFoundException e) {
						//should not happen
						//TODO: log
						e.printStackTrace();
					}
				}
				
				//add instructions to instruction container
				for (Entry<String, List<Instruction>> entry : instructions.entrySet()) {
					Statement s = statements.get( entry.getKey() );
					if (s instanceof InstructionContainer) {
						InstructionContainer container = (InstructionContainer) s;
						
						Class<?> clazz = container.getClass();
						
						try {
							Method method = clazz.getDeclaredMethod("addInstruction", Instruction.class);
							method.setAccessible(true);
							
							for (Instruction i : entry.getValue()) {
				            	method.invoke(container, i);
							}
						} catch (Exception e) {
							// TODO: log
							e.printStackTrace();
						}
					}
				}
				Statements.ready = true;
			}
		});
		
		scanner.start();
	}
	
	private static Map<String, Statement> statements = 
		new FastMap<String, Statement>();
	
	public static Statement get(String uri) {
		ready();
		
		return statements.get(uri);
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
