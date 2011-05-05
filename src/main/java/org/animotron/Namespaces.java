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
public enum Namespaces {
	
	ANIMO	(0, "animo", "animo"),
	 
	THE		( 1, "the", "animo/instance"),
	AN		( 2, "an", "animo/reference"),

	IS		(10, "is", "animo/relation/is"),
	HAVE	(11, "have", "animo/relation/have"),
	USE		(12, "use", "animo/relation/use"),

	IC		(20, "ic", "animo/connection"),

	ALL		(31, "all", "animo/query/all"),
	ANY		(32, "any", "animo/query/any"),
	GET		(33, "get", "animo/query/extract"),
	SELF	(34, "self", "animo/query/self"),

	PTRN	(40, "ptrn", "animo/pattern"),

	DO		(50, "do", "animo/perform"),

	LT		(61, "lt", "animo/compare/lt"), 
	GT		(62, "gt", "animo/compare/gt"), 
	EQ		(63, "eq", "animo/compare/eq"), 
	NE		(64, "ne", "animo/compare/ne"), 
	LE		(65, "le", "animo/compare/le"), 
	GE		(66, "ne", "animo/compare/ge");

	
	private final String prefix; 
	private final String namespace; 
	private final short id; 
	
	
	Namespaces (int id, String prefix, String namespace){
		this.id = (short)id;
		this.prefix = prefix;
		this.namespace = namespace;
	}
	
	public short id(){
		return id;
	}

	public String prefix(){
		return prefix;
	}
	
	public String namespace(){
		return namespace;
	}
	
	public boolean equals(String ns){
		return namespace.equals(ns);
	}
	
}
