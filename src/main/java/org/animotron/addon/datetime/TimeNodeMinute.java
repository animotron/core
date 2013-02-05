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
package org.animotron.addon.datetime;

import org.neo4j.graphdb.GraphDatabaseService;

/**
 * Minute object.
 * 
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * 
 */
public class TimeNodeMinute extends TimestampNode {
	
	public static final char SYMBOL = 'm';
	
//	static {
//		TimestampNode.addType(SYMBOL, TimeNodeMinute.class);
//	}
	
	//byte?
	private int minute;
	
	public TimeNodeMinute(GraphDatabaseService db, String name) {
		super(db, name);
		
		minute = Integer.valueOf(name.substring(1));
		if (!(minute >= 1 && minute <= 59)) {
			throw new IllegalArgumentException("Minute must be inrange between 1 and 59");
		}
	}
}
