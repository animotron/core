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
package org.animotron.exist.index;

import org.neo4j.graphdb.RelationshipType;

/**
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class RelationshipTypeTHE implements RelationshipType {
	
	private final static String prefix = RelationshipTypes.THE + ":"; 
	private String name;
	
	public RelationshipTypeTHE(String name){
		this.name = prefix + name;
	}

	@Override
	public String name() {
		return name;
	}
	
	public static boolean isSupertypeOf(RelationshipType type){
		return type.name().startsWith(prefix);
	}
	
}
