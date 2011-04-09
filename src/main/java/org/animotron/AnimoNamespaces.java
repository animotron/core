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

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public enum AnimoNamespaces {
	
	ANIMO	("animo", "animo"),
	 
	THE		("the", "animo/instance"),
	AN		("an", "animo/reference"),

	IC		("ic", "animo/connection"),

	ALL		("all", "animo/query/all"),
	ANY		("any", "animo/query/any"),
	GET		("get", "animo/query/extract"),
	SELF	("self", "animo/query/self"),

	PTRN	("ptrn", "animo/pattern"),

	DO		("do", "animo/perform"),

	IS		("is", "animo/relation/is"),
	USE		("use", "animo/relation/use"),
	HAVE	("have", "animo/relation/have"),

	LT		("lt", "animo/compare/lt"), 
	GT		("gt", "animo/compare/gt"), 
	EQ		("eq", "animo/compare/eq"), 
	NE		("ne", "animo/compare/ne"), 
	LE		("le", "animo/compare/le"), 
	GE		("ne", "animo/compare/ge");

	
	private final String prefix; 
	private final String namespace; 
	
	
	AnimoNamespaces (String prefix, String namespace){
		this.prefix = prefix;
		this.namespace = namespace;
	}
	
	public String prefix(){
		return prefix;
	}
	
	public String namespace(){
		return namespace;
	}
}
