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

import static org.animotron.graph.AnimoGraph.execute;
import static org.animotron.graph.AnimoGraph.shutdownDB;
import static org.junit.Assert.assertNotNull;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PipedInputStream;
import java.io.PipedOutputStream;
import java.util.Map;
import java.util.Map.Entry;

import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;

import junit.framework.Assert;

import org.animotron.graph.AnimoGraph;
import org.animotron.graph.CommonGraphBuilder;
import org.animotron.graph.GraphOperation;
import org.animotron.Reader;
import org.animotron.io.PipedInputObjectStream;
import org.animotron.manipulator.Evaluator;
import org.animotron.operator.THE;
import org.animotron.serializer.AnimoResultSerializer;
import org.animotron.serializer.StringResultSerializer;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.neo4j.graphdb.Relationship;

import com.ctc.wstx.stax.WstxOutputFactory;


/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class ATest {
	
	public static final WstxOutputFactory OUTPUT_FACTORY = new WstxOutputFactory();
	
	private static boolean cleanAfterTest = false;


	public static final String ANIMO_NSs = 
		"xmlns:the='animo/instance' " +
		"xmlns:an='animo/reference' " +
		
		"xmlns:ptrn='animo/pattern' " +
		
		"xmlns:have='animo/relation/have' " +
		"xmlns:use='animo/relation/use' " +
		"xmlns:is='animo/relation/is' " +

		"xmlns:ic='animo/connection' " +
		
		"xmlns:do='animo/perform' " +
		
		"xmlns:get='animo/query/extract' " +
		"xmlns:any='animo/query/any' " +
		"xmlns:all='animo/query/all' " +
		
		"xmlns:eq='animo/compare/eq' " +

		"xmlns:op='animo/operation' " +
		"xmlns:string='animo/string' " +
		"xmlns:ml='animo/ml' " +
		
		"xmlns:Q='animo/quantity' ";

	protected void store(final Map<String, String> nameDataMap) throws XMLStreamException {
		XMLStreamException e = 
			execute(new GraphOperation<XMLStreamException>() {
				@Override
				public XMLStreamException execute() {
			        for (Entry<String, String> entry : nameDataMap.entrySet()) {
			        	try {
							CommonGraphBuilder.build(entry.getValue());
						} catch (XMLStreamException e) {
							return e;
						}
			        }
			        return null;
				}
			});
		if (e != null) throw e;
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
					System.out.print((char)buffer[i]);
					b.append((char)buffer[i]);
				}
			} 
		} finally { 
			stream.close(); 
		}
		
		Assert.assertEquals("check evaluation result", expecteds, b.toString());
	}

	protected void assertEquals(String the, String expecteds) throws IOException {
        Relationship op = THE._.get(the);
        assertNotNull(op);

//        toConsole(
//    		Calculator.eval(op)
//		);
        
    	InputStream stream = Reader.read(op);
        assertEquals(stream, expecteds);
	}
	
	protected void assertAnimo(Relationship op, String expecteds) throws IOException {
        assertNotNull(op);

        System.out.println("Animo result serializer...");
        
		PipedInputStream in = new PipedInputStream();
		PipedOutputStream out = new PipedOutputStream(in);

        XMLStreamWriter writer;
		try {
			writer = OUTPUT_FACTORY.createXMLStreamWriter(out);
		} catch (XMLStreamException e) {
			throw new IOException(e);
		}
        
        AnimoResultSerializer serializer = new AnimoResultSerializer(writer);
        serializer.serialize(op);
        assertEquals(in, "<?xml version='1.0' encoding='UTF-8'?>"+expecteds);
	}

	protected void assertString(String the, String expecteds) throws IOException {
        Relationship op = THE._.get(the);
        assertNotNull(op);

        toConsole(
    		Evaluator._.markExecute(op, new Catcher())
		);
        
        System.out.println("String result serializer...");
        StringResultSerializer serializer = new StringResultSerializer();
        serializer.serialize(op);
        Assert.assertEquals(serializer.getString(), expecteds);
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
    	AnimoGraph.startDB("data");
    }

    @AfterClass
    public static void stopDB() {
    	shutdownDB();
    	
    	if (cleanAfterTest) {
    		;//delete folder?
    	}
    }
}
