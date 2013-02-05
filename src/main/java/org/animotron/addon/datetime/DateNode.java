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

import org.joda.time.DateTime;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;
import org.neo4j.graphdb.GraphDatabaseService;

/**
 * Date object.
 * 
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * 
 */
public class DateNode extends TimestampNode {
	
	public static final char SYMBOL = 'D';
	
//	static {
//		TimestampNode.addType(SYMBOL, DateNode.class);
//	}
	
	private static DateTimeFormatter matcher = DateTimeFormat.forPattern("yyyy-MM-dd");

	private DateTime date;
	
	public DateNode(GraphDatabaseService db, String name) {
		super(db, name);
		
		date = matcher.parseDateTime(name.substring(1));
	}
	
	public DateTime getValue() {
		return date;
	}
}