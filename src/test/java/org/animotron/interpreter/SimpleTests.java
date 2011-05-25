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

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.LinkedHashMap;
import java.util.Map;

import org.animotron.exist.AbstractTest;
import org.animotron.graph.Reader;
import org.animotron.io.PipedInputObjectStream;
import org.animotron.operator.THE;
import org.exist.EXistException;
import org.exist.storage.DBBroker;
import org.junit.Test;
import org.neo4j.graphdb.Relationship;


/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class SimpleTests extends AbstractTest {
	
	private static final String THE_A = 
		"<the:A "+ANIMO_NSs+"/>";
	
	private static final String THE_B = 
		"<the:B "+ANIMO_NSs+">" +
		"	<have:A>a@b</have:A>"+
		"</the:B>";

	private static final String THE_C = 
		"<the:C "+ANIMO_NSs+">" +
		"	<get:A>" +
		"		<an:B/>" +
		"	</get:A>" +
		"</the:C>";

	@Test
	public void testGet() throws IOException {
        System.out.println("Test 'get' ...");
        
        if (firstRun) {
	        Map<String, String> nameDataMap = new LinkedHashMap<String, String>();
	        nameDataMap.put("A.xml", THE_A);
	        nameDataMap.put("B.xml", THE_B);
	        nameDataMap.put("C.xml", THE_C);
	        
	        configureAndStore(COLLECTION_CONFIG, nameDataMap);
        }
        
        DBBroker broker = null;
        try {
            broker = pool.get(pool.getSecurityManager().getSystemSubject());
            assertNotNull(broker);
            
            Relationship op = THE.getInstance().relationship("C");
            
            assertNotNull(op);

            //System.out.println("get:A an:B");
            //PipedInputObjectStream instream = 
        	Calculator.eval(op);
            //toConsole(instream);
            
        	InputStream stream = Reader.read(op);
            toConsole(stream);
            
            //RESULT: <the:C><have:A>a@b</have:A></the:C>
            
//            assertEquals(1, set.getItemCount());
//            
//            Set<String> expect = new HashSet<String>();
//            expect.add("C");
//            
//            for (int i = 0; i < set.getItemCount(); i++) {
//            	String name = set.get(i).getNode().getLocalName();
//            	assertTrue(name, expect.remove(name));
//            }
        } catch (EXistException e) {
			e.printStackTrace();
			fail(e.getMessage());
		} finally {
        	pool.release(broker);
        }
        //System.out.println("done.");
	}
	
	private void toConsole(PipedInputObjectStream instream) throws IOException {
		if (instream == null) return;
		
		Object n; 
		while ((n = instream.read()) != null) {
			System.out.print(n.toString());
		} 
	}

	private void toConsole(InputStream stream) throws IOException {
		if (stream == null) return;
		
		char[] buffer = new char[1024]; 
		try { 
			BufferedReader reader = new BufferedReader(new InputStreamReader(stream, "UTF-8")); 

			int n; 
			while ((n = reader.read(buffer)) != -1) {
				for (int i = 0; i < n; i++) {
					System.out.print((char)buffer[i]);
				}
			} 
		} finally { 
			stream.close(); 
		} 
	}
}
