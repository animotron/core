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
public interface Namespaces {
	
	public String the	= "animo/instance";
	public String an	= "animo/reference";

	public String ic	= "animo/connection";

	public String all	= "animo/query/all";
	public String any	= "animo/query/any";
	public String self	= "animo/query/self";
	public String get	= "animo/query/extract";

	public String ptrn	= "animo/pattern";

	//public String do	= "animo/perform";

	public String is	= "animo/relation/is";
	public String use	= "animo/relation/use";
	public String have	= "animo/relation/have";

	public String lt	= "animo/compare/lt"; 
	public String gt	= "animo/compare/gt"; 
	public String eq	= "animo/compare/eq"; 
	public String ne	= "animo/compare/ne"; 
	public String le	= "animo/compare/le"; 
	public String ge	= "animo/compare/ge";
}
