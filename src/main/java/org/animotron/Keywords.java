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
 * @author <a href="mailto:gazdovskyd@gmail.com">E</a>
 *
 */
public enum Keywords {
	
	DO_XQUERY			("xquery", Namespaces.DO),
	DO_XSLT				("xslt", Namespaces.DO),
	DO_SKIP				("skip", Namespaces.DO),
	 
	AN_EMPTY			("empty", Namespaces.AN),
	AN_SELF				("self", Namespaces.AN),
	AN_CONTENT  		("content", Namespaces.AN),
	
	SELF_INSTANCE		("instance", Namespaces.SELF),

	USE_FLOW_STACK		("flow-stack", Namespaces.USE),
	USE_GLOBAL_CONTEXT	("repository", Namespaces.USE),
	USE_LOCAL_CONTEXT	("context", Namespaces.USE),
	USE_CONTEXT			("CONTEXT", Namespaces.USE),
	USE_CONTEXT_STACK	("stack", Namespaces.USE);
	
	private final String keyword; 
	private final Namespaces namespace; 
	
	
	Keywords (String keyword, Namespaces namespace){
		this.keyword = keyword;
		this.namespace = namespace;
	}
	
	public String keyword(){
		return keyword;
	}
	
	public String namespace(){
		return namespace.namespace();
	}
	
	public String prefix(){
		return namespace.prefix();
	}
}
