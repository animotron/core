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
package org.animotron.graph;

import org.neo4j.graphdb.RelationshipType;

import java.util.Map;
import java.util.WeakHashMap;

/**
 * RelationType implementation for animo graph needs.
 * 
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class AnimoRelationshipType implements RelationshipType {
	
	private static Map<String, RelationshipType> map = new WeakHashMap<String, RelationshipType>(); 
	
	private final String name;
	
	private AnimoRelationshipType(final String name) {
		this.name = name;
	}

	private static String name (String prefix, String name) {
		return prefix + ":" + name;
	}

	public static RelationshipType get(String prefix, String name) {
        if (name == null) {
            return get(prefix);
        }
		return get(name(prefix, name));
	}

	public static RelationshipType get(String name) {
		RelationshipType rt = map.get(name);
		if (rt == null) {
			rt = new AnimoRelationshipType(name);
			map.put(name, rt);
		}
		return rt;
	}
	
	@Override
	public String name() {
		return name;
	}

	//XXX: move away
	public static boolean isSupertypeOf(RelationshipType type){
		return type.name().startsWith("THE:");
	}
	
	public String toString() {
		return ""+name+"";
	}
}
