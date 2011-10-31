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

import org.neo4j.graphdb.PropertyContainer;


/**
 * @author <a href="mailto:gazdovskyd@gmail.com">Evgeny Gazdovsky</a>
 *
 */
//XXX: move to org.animotron.graph ?
public enum Properties  {
	
	NAME, SOURCE, NAMESPACE, VALUE, HASH, PREFIX, CONTENT, RID, CID, BIN;
	
	public Object get(PropertyContainer container) {
		return container.getProperty(name());
	}
	
	public void set(PropertyContainer container, Object value) {
		if (value != null) 
			container.setProperty(name(), value);
	}
	
	public boolean has(PropertyContainer container) {
		return container.hasProperty(name());
	}

}
