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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;

import org.junit.Test;


/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class DateTimeTests {
	
	@Test
	public void formats() throws IllegalArgumentException {
		//year
		check("Y1975");
		
		//mounth
		check("M02");
		
		try {
			check("M14");
			assertFalse(true);
		} catch (IllegalArgumentException e) {
		}
		
		//hour
		check("h03");
		check("h3");

		try {
			check("h25");
			assertFalse(true);
		} catch (IllegalArgumentException e) {
		}

		//minute
		check("m03");
		check("m3");

		try {
			check("m60");
			assertFalse(true);
		} catch (IllegalArgumentException e) {
		}

		//second
		check("s03");
		check("s3");

		try {
			check("s60");
			assertFalse(true);
		} catch (IllegalArgumentException e) {
		}

		//time
		check("T03-10");
		check("T3-2");

		try {
			check("T24-1");
			assertFalse(true);
		} catch (IllegalArgumentException e) {
		}

		try {
			check("T23-60");
			assertFalse(true);
		} catch (IllegalArgumentException e) {
		}

		check("P3s");
	}
	
	private void check(String str) throws IllegalArgumentException {
		TimestampNode t = TimestampNode.getInstance(null, str);
		
		assertEquals(str, t.getProperty("NAME"));
	}

}
