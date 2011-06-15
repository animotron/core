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

import java.util.List;

import org.neo4j.graphdb.Relationship;

import javolution.util.FastList;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class Manipulators implements Creative, Destructive {
	
	public static Manipulators _ = new Manipulators(); 
	
	private static List<Creative> creative = new FastList<Creative>();
	private static List<Destructive> descructive = new FastList<Destructive>();
	
	static {
		creative.add(Calculator._);
		descructive.add(GC._);
	}
	
	private Manipulators() {}

	public void creativePush(Relationship op) {
		for (Creative m : creative) {
			m.creativePush(op);
		}
	}

	public void destructivePush(Relationship op) {
		for (Destructive m : descructive) {
			m.destructivePush(op);
		}
	}

}
