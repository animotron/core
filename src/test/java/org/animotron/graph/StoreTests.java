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
package org.animotron.graph;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.fail;

import java.util.LinkedHashMap;
import java.util.Map;

import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;

import org.animotron.ATest;
import org.animotron.graph.stax.StAXGraphSerializer;
import org.animotron.operator.THE;
import org.junit.Test;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.Transaction;


/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class StoreTests extends ATest {
	
	private static final String THE_A = 
		"<the:A "+ANIMO_NSs+">" +
		"	<x:some x:a=\"b\" xmlns:x=\"y\" z=\"w\"/>" +
		"</the:A>";
	
	private static final String THE_B = 
		"<the:B "+ANIMO_NSs+">" +
		"	<is:A/>" +
		"	<have:C>" +
		"		<x:some x:a=\"b\" xmlns:x=\"y\" z=\"w\"/>" +
		"		<another>" +
		"			<one-more/>" +
		"		</another>" +
		"	</have:C>" +
		"	<an:D>" +
		"		<is:A/>" +
		"		<use:F/>" +
		"		<have:C>" +
		"			<have:E>" +
		"				<x:some x:a=\"b\" xmlns:x=\"y\" z=\"w\"/>" +
		"			</have:E>" +
		"		</have:C>" +
		"	</an:D>" +
		"</the:B>";

	@Test
	public void storeAndSerialize() throws XMLStreamException {
        System.out.println("Test processing flow interator ...");
        
        Map<String, String> nameDataMap = new LinkedHashMap<String, String>();
        nameDataMap.put("A.xml", THE_A);
        nameDataMap.put("B.xml", THE_B);
        
        store(nameDataMap);
        System.out.println("loaded ...");
        
        Transaction tx = AnimoGraph.beginTx();
        
        try { 
	        Relationship r = THE.getInstance().relationship("B");
	            
	        assertNotNull(r);
	        
	        XMLStreamWriter writer = OUTPUT_FACTORY.createXMLStreamWriter(System.out);
	        
	        StAXGraphSerializer serializer = new StAXGraphSerializer(writer);
	        serializer.serialize(r);
	            
//	        String[] must = new String[] {"the:B", "have:B", "some", "another"};
//	        int i = 0;
//            ProcessingFlowIterator it = new ProcessingFlowIterator(node);
//            while (it.hasNext()) {
//            	System.out.println(it.next());
            	//System.out.println(AnimoGraph.getNodeProxy(it.next()).getNode().getNodeName());
            	//assertEquals("on "+i+" step", must[i], AnimoGraph.getNodeProxy(it.next()).getNode().getNodeName());
//            	i++;
//            }
	        
        } catch (XMLStreamException e) {
        	e.printStackTrace();
			fail(e.toString());
        } finally {
        	tx.finish();
        }
            
        System.out.println("done.");
	}
}
