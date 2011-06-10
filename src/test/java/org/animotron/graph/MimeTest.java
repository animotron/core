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

import java.io.ByteArrayInputStream;
import java.io.InputStream;

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
public class MimeTest extends ATest {
	
	private static final String MIME = 
		"<the:text-plain " + ATest.ANIMO_NSs + ">" +
		"   <is:mime-type/>" +
		"   <is:text/>" +
		"   <have:type>text/plain</have:type>" +
		"   <have:name>Plain text</have:name>" +
		"   <have:extension>txt</have:extension>" +
		"</the:text-plain>";
	
	private static final String STATEMENT = 
		"<any:mime-type " + ATest.ANIMO_NSs + ">" +
		"  	<eq:extension>txt</eq:extension>" +
		"</any:mime-type>";
	
	@Test
	public void storeAndSerializeResult() throws XMLStreamException {
        System.out.println("Test processing flow interator ...");
        
        CommonGraphBuilder.build(MIME);
        CommonGraphBuilder.build(STATEMENT);

        
        System.out.println("loaded ...");
        
//        Transaction tx = AnimoGraph.beginTx();
//        try { 
//	        Relationship r = THE._.relationship("B");
//	        assertNotNull(r);
//	        XMLStreamWriter writer = OUTPUT_FACTORY.createXMLStreamWriter(System.out);
//	        StAXGraphSerializer serializer = new StAXGraphSerializer(writer);
//	        serializer.serialize(r);
//	        tx.success();
//	        
//        } catch (Exception e) {
//        	e.printStackTrace();
//			fail(e.toString());
//        } finally {
//        	tx.finish();
//        }
            
        System.out.println("done.");
	}
}
