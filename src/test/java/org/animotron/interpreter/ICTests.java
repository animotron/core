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
package org.animotron.interpreter;

import java.io.IOException;
import java.util.LinkedHashMap;
import java.util.Map;

import javax.xml.stream.XMLStreamException;

import org.animotron.ATest;
import org.junit.Test;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class ICTests extends ATest {
	
	private static final String THE_A = 
		"<the:A "+ANIMO_NSs+"/>";
	
	private static final String THE_B = 
		"<the:B "+ANIMO_NSs+">" +
		"	<ic:A>.</ic:A>" +
		"</the:B>";

	private static final String THE_C = 
		"<the:C "+ANIMO_NSs+">" +
		"	<is:B/>" +
		"</the:C>";

	private static final String THE_D = 
		"<the:D "+ANIMO_NSs+">" +
		"	<get:A>" +
		"		<an:C/>" +
		"	</get:A>" +
		"</the:D>";


	@Test
	public void testIC() throws IOException, XMLStreamException {
        System.out.println("Test 'IC' ...");
        
        if (firstRun) {
	        Map<String, String> nameDataMap = new LinkedHashMap<String, String>();
	        nameDataMap.put("A.xml", THE_A);
	        nameDataMap.put("B.xml", THE_B);
	        nameDataMap.put("C.xml", THE_C);
	        nameDataMap.put("D.xml", THE_D);
	        
	        store(nameDataMap);
        }
        
        //System.out.println("get:A an:C");
        assertString("D", ".");
//        assertEquals("D", "<the:D><have:A>.</have:A></the:D>");

        //System.out.println("done.");
	}
	
}
