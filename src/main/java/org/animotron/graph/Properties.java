/*
 *  Copyright (C) 2011-2012 The Animo Project
 *  http://animotron.org
 *  	
 *  This file is part of Animotron.
 *  
 *  Animotron is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU Affero General Public License as 
 *  published by the Free Software Foundation, either version 3 of 
 *  the License, or (at your option) any later version.
 *  
 *  Animotron is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Affero General Public License for more details.
 *  
 *  You should have received a copy of 
 *  the GNU Affero General Public License along with Animotron.  
 *  If not, see <http://www.gnu.org/licenses/>.
 */
package org.animotron.graph;

import org.neo4j.graphdb.PropertyContainer;


/**
 * @author <a href="mailto:gazdovskyd@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public enum Properties  {
	
	NAME, VALUE, HASH, RID, CID, CONTEXT, TYPE, TO_NODE, CREATED, MODIFIED;
	
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
    
    public boolean equals(String s) {
        return name().equals(s);
    }

}
