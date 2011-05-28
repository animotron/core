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

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.fail;

import java.io.IOException;
import java.io.InputStream;
import java.util.LinkedHashMap;
import java.util.Map;

import org.animotron.exist.AbstractTest;
import org.animotron.graph.Reader;
import org.animotron.operator.THE;
import org.exist.EXistException;
import org.exist.storage.DBBroker;
import org.junit.Test;
import org.neo4j.graphdb.Relationship;


/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class ContextTests extends AbstractTest {
	
	private static final String THE_A = 
		"<the:A "+ANIMO_NSs+"/>";
	
	private static final String THE_B = 
		"<the:B "+ANIMO_NSs+">" +
		"	<is:A/>"+
		"</the:B>";

	private static final String THE_C = 
		"<the:C "+ANIMO_NSs+">" +
		"	<is:B/>" +
		"</the:C>";

	private static final String THE_D = 
		"<the:D "+ANIMO_NSs+">" +
		"	<any:A/>" +
		"</the:D>";

	private static final String THE_E = 
		"<the:E "+ANIMO_NSs+">" +
		"	<an:D>" +
		"		<use:B/>" +
		"	</an:D>" +
		"</the:E>";

	private static final String THE_F = 
		"<the:F "+ANIMO_NSs+">" +
		"	<an:D>" +
		"		<use:C/>" +
		"	</an:D>" +
		"</the:F>";

	private static final String THE_G = 
		"<the:G "+ANIMO_NSs+">" +
		"	<an:E>" +
		"		<use:A/>" +
		"	</an:E>" +
		"</the:G>";

	@Test
	public void testGet() throws IOException {
        System.out.println("Test 'get' ...");
        
        if (firstRun) {
	        Map<String, String> nameDataMap = new LinkedHashMap<String, String>();
	        nameDataMap.put("A.xml", THE_A);
	        nameDataMap.put("B.xml", THE_B);
	        nameDataMap.put("C.xml", THE_C);
	        nameDataMap.put("D.xml", THE_D);
	        nameDataMap.put("E.xml", THE_E);
	        nameDataMap.put("F.xml", THE_F);
	        nameDataMap.put("G.xml", THE_G);
	        
	        configureAndStore(COLLECTION_CONFIG, nameDataMap);
        }
        
        DBBroker broker = null;
        try {
            broker = pool.get(pool.getSecurityManager().getSystemSubject());
            assertNotNull(broker);
            
            //System.out.println("any:A");
            assertEquals("D", "<the:D><the:A></the:A><the:B></the:B><the:C></the:C></the:D>");

            //System.out.println("an:D use:B");
            assertEquals("E", "<the:E><the:D><the:B></the:B><the:C></the:C></the:D></the:E>");

        } catch (EXistException e) {
			e.printStackTrace();
			fail(e.getMessage());
		} finally {
        	pool.release(broker);
        }
        //System.out.println("done.");
	}
	
	private void assertEquals(String the, String expecteds) throws IOException {
        Relationship op = THE.getInstance().relationship(the);
        assertNotNull(op);

        toConsole(
    		Calculator.eval(op)
		);
        
    	InputStream stream = Reader.read(op);
        assertEquals(stream, expecteds);
	}
}
