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

import org.exist.dom.QName;

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
	
	SELF_INSTANCE		("instance", Namespaces.SELF);

	private final QName qname; 
	private final String keyword; 
	private final Namespaces namespace; 
	
	Keywords (String keyword, Namespaces namespace){
		this.keyword = keyword;
		this.namespace = namespace;
		this.qname = new QName(keyword, namespace.namespace());
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
	
	public QName QName(){
		return qname;
	}
	
	public boolean equals(QName qname){
		return this.qname.equals(qname);
	}
	
	public boolean equals(String keyword, String namespace){
		return this.namespace.equals(namespace) && this.keyword.equals(keyword);
	}
	
}
