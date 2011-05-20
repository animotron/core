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
package org.animotron.addon.datetime;

import org.joda.time.Period;
import org.joda.time.format.PeriodFormatter;
import org.joda.time.format.PeriodFormatterBuilder;
import org.neo4j.graphdb.GraphDatabaseService;

/**
 * Time object.
 * 
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * 
 */
public class TimeNode extends TimestampNode {
	
	public static final char SYMBOL = 'T';
	
//	static {
//		TimestampNode.addType(SYMBOL, TimeNode.class);
//	}
	
	private static PeriodFormatter matcher = 
		new PeriodFormatterBuilder()
			.appendHours()
			.appendSeparator("-")
			.appendMinutes()
			.appendSeparator("-")
			.appendSeconds()
			.toFormatter();
	
	private Period time;
	
	public TimeNode(GraphDatabaseService db, String name) {
		super(db, name);
		
		time = matcher.parsePeriod(name.substring(1));
		
		int hour = time.getHours();
		if (!(hour >= 0 && hour <= 23)) {
			throw new IllegalArgumentException("Hour in time must be inrange between 0 and 23");
		}

		int minutes = time.getMinutes();
		if (!(minutes >= 0 && minutes <= 59)) {
			throw new IllegalArgumentException("Minutes in time must be inrange between 0 and 59");
		}

		int seconds = time.getSeconds();
		if (!(seconds >= 0 && seconds <= 59)) {
			throw new IllegalArgumentException("Seconds in time must be inrange between 0 and 59");
		}
	}
}
