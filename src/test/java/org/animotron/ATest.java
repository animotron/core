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
package org.animotron;

import static org.junit.Assert.assertNotNull;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.StringReader;
import java.util.Map;
import java.util.Map.Entry;

import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;

import junit.framework.Assert;

import org.animotron.graph.AnimoGraph;
import org.animotron.graph.Reader;
import org.animotron.graph.stax.StAXGraphBuilder;
import org.animotron.interpreter.Calculator;
import org.animotron.io.PipedInputObjectStream;
import org.animotron.operator.THE;
import org.junit.*;
import org.neo4j.graphdb.Relationship;


/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class ATest {
	
	private static boolean cleanAfterTest = false;
	
	public static final String ANIMO_NSs = 
		"xmlns:the='animo/instance' " +
		"xmlns:an='animo/reference' " +
		
		"xmlns:ptrn='animo/pattern' " +
		
		"xmlns:have='animo/relation/have' " +
		"xmlns:use='animo/relation/use' " +
		"xmlns:is='animo/relation/is' " +

		"xmlns:do='animo/perform' " +
		
		"xmlns:get='animo/query/extract' " +
		"xmlns:any='animo/query/any' " +
		"xmlns:all='animo/query/all' " +
		
		"xmlns:op='animo/operation' " +
		
		"xmlns:Q='animo/quantity' ";

	protected void store(Map<String, String> nameDataMap) throws XMLStreamException {
		
		XMLInputFactory factory = XMLInputFactory.newInstance();

    	String data = null; 
        for (Entry<String, String> entry : nameDataMap.entrySet()) {
        	data = entry.getValue(); 
        	
        	XMLStreamReader streamReader = factory.createXMLStreamReader(new StringReader(data));
        	
        	new StAXGraphBuilder(streamReader).build();
        }
    }

	protected void toConsole(PipedInputObjectStream instream) throws IOException {
		if (instream == null) return;
		
		Object n; 
		while ((n = instream.read()) != null) {
			System.out.print(n.toString());
		} 
	}

	protected void toConsole(InputStream stream) throws IOException {
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

	protected void assertEquals(InputStream stream, String expecteds) throws IOException {
		if (stream == null) return;
		
		StringBuilder b = new StringBuilder(expecteds.length()); 
		
		char[] buffer = new char[1024]; 
		try { 
			BufferedReader reader = new BufferedReader(new InputStreamReader(stream, "UTF-8")); 

			int n; 
			while ((n = reader.read(buffer)) != -1) {
				for (int i = 0; i < n; i++) {
					b.append((char)buffer[i]);
				}
			} 
		} finally { 
			stream.close(); 
		}
		
		Assert.assertEquals("check evaluation result", expecteds, b.toString());
	}

	protected void assertEquals(String the, String expecteds) throws IOException {
        Relationship op = THE.getInstance().relationship(the);
        assertNotNull(op);

        toConsole(
    		Calculator.eval(op)
		);
        
    	InputStream stream = Reader.read(op);
        assertEquals(stream, expecteds);
	}
	
    protected boolean firstRun = false;

    @Before
    public void setup() {
    	firstRun = true;
    }

    @After
    public void cleanup() {
    }

    @BeforeClass
    public static void startDB() {
    }

    @AfterClass
    public static void stopDB() {
    	AnimoGraph.shutdownDB();
    	
    	if (cleanAfterTest) {
    		;//delete folder?
    	}
    }
}
