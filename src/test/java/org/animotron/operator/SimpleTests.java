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
package org.animotron.operator;

import static org.animotron.graph.AnimoGraph.beginTx;
import static org.animotron.graph.AnimoGraph.finishTx;
import static org.junit.Assert.assertNotNull;

import java.io.IOException;
import java.io.InputStream;
import java.util.LinkedHashMap;
import java.util.Map;

import javax.xml.stream.XMLStreamException;

import org.animotron.ATest;
import org.animotron.graph.Reader;
import org.animotron.manipulator.Evaluator;
import org.animotron.operator.THE;
import org.junit.Test;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.Transaction;


/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class SimpleTests extends ATest {
	
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
	public void testGet() throws IOException, XMLStreamException {
        System.out.println("Test 'get' ...");
        
        if (firstRun) {
	        Map<String, String> nameDataMap = new LinkedHashMap<String, String>();
	        nameDataMap.put("A.xml", THE_A);
	        nameDataMap.put("B.xml", THE_B);
	        nameDataMap.put("C.xml", THE_C);
	        
	        store(nameDataMap);
        }
        
        Transaction tx = beginTx();
        
        try {
	        Relationship op = THE._.get("C");
	        
	        assertNotNull(op);
	        
	        System.out.println(op);
	
	        //System.out.println("get:A an:B");
	        toConsole(Evaluator._.execute(op));
	        
	    	InputStream stream = Reader.read(op);
	        assertEquals(stream, "<the:C><have:A>a@b</have:A></the:C>");
        
        } finally {
        	finishTx(tx);
        }
            
        //System.out.println("done.");
	}
	
}
