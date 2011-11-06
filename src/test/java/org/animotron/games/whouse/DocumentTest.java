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
package org.animotron.games.whouse;

import org.animotron.ATest;
import org.junit.Test;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class DocumentTest extends ATest {

	@Test
	public void test() throws Exception {
		
		testAnimo("the user1 have name \"user1\".");
		testAnimo("the user2 have name \"user1\".");

		testAnimo("the number have name \"number\".");
		testAnimo("the date have name \"date\".");
		testAnimo("the owner have name \"owner\".");

		testAnimo("the document (have date) (have number) (have owner).");
		
		testAnimo("the doc3 (is document) (have date D \"2011-11-01\") (have number N D 3) (have owner an user1).");
		testAnimo("the doc5 (is document) (have date D \"2011-11-02\") (have number N D 5) (have owner an user1).");
		testAnimo("the doc7 (is document) (have date D \"2011-11-03\") (have number N D 7) (have owner an user2).");
		
		testAnimo("the table (have column) (have row) (ic html \\table (\\tr each (get column) (\\th get name)) (each (get row) (\\tr each (get column) (\\td get name) )).");
		
		testAnimo("the journal (is table) (have column (an date) (an number)) (have row all document).");
		
		testAnimoResult("get html an journal", 
			"<table>" +
				"<tr>" +
					"<th>date</th>" +
					"<th>number</th>" +
				"</tr>" +
				"<tr>" +
					"<td>2011-11-01</td>" +
					"<td>3</td>" +
				"</tr>" +
				"<tr>" +
					"<td>2011-11-02</td>" +
					"<td>5</td>" +
				"</tr>" +
				"<tr>" +
					"<td>2011-11-03</td>" +
					"<td>7</td>" +
				"</tr>" +
			"</table>");
	}
}
