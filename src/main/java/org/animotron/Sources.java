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

import java.util.HashSet;
import java.util.Set;

import org.w3c.dom.Element;

/**
 * @author <a href="mailto:gazdovskyd@gmail.com">E</a>
 *
 */
public class Sources {
	
	private static String NAME = "source";
	private static Set<String>SOURCES = new HashSet<String>(5);
	
	static {
		SOURCES.add("flow-stack");
		SOURCES.add("context");
		SOURCES.add("CONTEXT");
		SOURCES.add("stack");
	}
	
	public static String getSource(Element element){
		String src = element.getAttribute(NAME);
		return SOURCES.contains(src) ? src : null;
	}
}
