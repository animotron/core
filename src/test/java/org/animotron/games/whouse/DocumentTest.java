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
package org.animotron.games.whouse;

import org.animotron.ATest;
import org.animotron.expression.AnimoExpression;
import org.junit.Test;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class DocumentTest extends ATest {

	@Test
	public void test() throws Throwable {
		
		testAnimo("def D2011-11-01 name \"1 November 2011\".");
		testAnimo("def D2011-11-02 name \"2 November 2011\".");
		testAnimo("def D2011-11-03 name \"3 November 2011\".");

		testAnimo("def N3 name \"3\".");
		testAnimo("def N5 name \"5\".");
		testAnimo("def N7 name \"7\".");

		testAnimo("def user1 name \"user1\".");
		testAnimo("def user2 name \"user1\".");

		testAnimo("def number name \"number\".");
		testAnimo("def date name \"date\".");
		testAnimo("def owner name \"owner\".");

		testAnimo("def document (date) (number) (owner).");

		testAnimo("def doc3 (document) (date D2011-11-01) (number N3) (owner user1).");
		testAnimo("def doc5 (document) (date D2011-11-02) (number N5) (owner user1).");
		testAnimo("def doc7 (document) (date D2011-11-03) (number N7) (owner user2).");
		
		testAnimo("def table " +
					"(column) " +
					"(row) " +
					"(html " +
						"\\table " +
							"(\\tr each (get column) (\\th get name)) " +
							"(each (get row) (\\tr each (get column) (\\td get name get (this column) (this row))))" +
					").");
		
		testAnimo("def journal (table) (column (date) (number)) (row all document).");

		assertAnimoResult(
            new AnimoExpression("get html journal"),
			"<table>" +
				"<tr>" +
					"<th>date</th>" +
					"<th>number</th>" +
				"</tr>" +
				"<tr>" +
					"<td>1 November 2011</td>" +
					"<td>3</td>" +
				"</tr>" +
				"<tr>" +
					"<td>2 November 2011</td>" +
					"<td>5</td>" +
				"</tr>" +
				"<tr>" +
					"<td>3 November 2011</td>" +
					"<td>7</td>" +
				"</tr>" +
			"</table>");

	}
}
