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
package org.animotron;

import com.ctc.wstx.stax.WstxOutputFactory;
import junit.framework.Assert;
import org.animotron.expression.AnimoExpression;
import org.animotron.graph.serializer.BinarySerializer;
import org.animotron.graph.serializer.CachedSerializer;
import org.animotron.graph.serializer.DigestSerializer;
import org.junit.After;
import org.junit.Before;
import org.neo4j.graphdb.Relationship;

import java.io.*;

import static org.animotron.graph.AnimoGraph.shutdownDB;
import static org.animotron.graph.AnimoGraph.startDB;
import static org.animotron.graph.Properties.HASH;
import static org.junit.Assert.assertNotNull;


/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public abstract class ATest {

    public static final String DATA_FOLDER = "data-test";
	
	public static final WstxOutputFactory OUTPUT_FACTORY = new WstxOutputFactory();

	protected void testAnimo(String exp) throws Exception {
        testAnimo(exp, exp);
    }

	protected void testAnimo(String in, String out) throws Exception {
        AnimoExpression expression = new AnimoExpression(in);
        assertEquals((byte[]) HASH.get(expression), DigestSerializer._.serialize(expression));
        assertAnimo(expression, out);
    }

	protected void testAnimoResult(String exp, String res) throws Exception {
		testAnimoResult(exp, exp, res);
    }

	protected void testAnimoResult(String in, String out, String res) throws Exception {
        AnimoExpression expression = new AnimoExpression(in);
        assertEquals((byte[]) HASH.get(expression), DigestSerializer._.serialize(expression));
        assertAnimo(expression, out);

        assertAnimoResult(expression, res);
    }


	protected void toConsole(InputStream stream) throws IOException {
		if (stream == null) return;
		
		char[] buffer = new char[1024]; 
		try { 
			BufferedReader reader = new BufferedReader(new InputStreamReader(stream, "UTF-8")); 

			int n; 
			while ((n = reader.read(buffer)) != -1) {
				for (int i = 0; i < n; i++) {
					System.out.print(buffer[i]);
				}
			} 
		} finally { 
			stream.close(); 
		} 
	}

    protected void assertEquals(byte[] a, byte[] b) {
        Assert.assertEquals(a.length, b.length);
        for (int i = 0; i < a.length; i++)
            Assert.assertEquals(a[i], b[i]);
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
					System.out.print(buffer[i]);
					b.append(buffer[i]);
				}
			} 
		} finally { 
			stream.close(); 
		}
		
		Assert.assertEquals("check evaluation result", expecteds, b.toString());
	}

    protected void assertAnimo(Relationship op, String expected) throws IOException {
        assertAnimo(op, expected, false);
    }

    protected void assertAnimo(Relationship op, String expected, boolean pretty) throws IOException {
        assertNotNull(op);

        System.out.println("Animo serializer...");
        String result = pretty ? CachedSerializer.PRETTY_ANIMO.serialize(op) : CachedSerializer.ANIMO.serialize(op);
        System.out.println(result);
        Assert.assertEquals("", expected, result);

        System.out.println();
    }

    protected void assertAnimoResult(String op, String expected) throws IOException {
        assertAnimoResult(new AnimoExpression(op), expected, false);
    }

    protected void assertAnimoResult(Relationship op, String expected) throws IOException {
        assertAnimoResult(op, expected, false);
    }

    protected void eval(Relationship op) throws IOException {
    	CachedSerializer.ANIMO_RESULT.serialize(op);
    }

    protected void assertAnimoResult(Relationship op, String expected, boolean pretty) throws IOException {
        assertNotNull(op);

        System.out.println("Animo result serializer...");
        String result = pretty ? CachedSerializer.PRETTY_ANIMO_RESULT.serialize(op) : CachedSerializer.ANIMO_RESULT.serialize(op);
        System.out.println(result);
        Assert.assertEquals("", expected, result);

        System.out.println();
    }
    
    protected void assertAnimoResultOneStep(String op, String expected) throws IOException {
    	assertAnimoResultOneStep(new AnimoExpression(op), expected);
    }

    protected void assertAnimoResultOneStep(Relationship op, String expected) throws IOException {
        assertNotNull(op);
        System.out.println("One step Animo result serializer...");
        String result = CachedSerializer.ANIMO_RESULT_ONE_STEP.serialize(op);
        System.out.println(result);
        Assert.assertEquals("", expected, result);
        System.out.println();
    }


    protected void assertXMLResult(Relationship op, String expected) throws IOException {
        assertNotNull(op);
        System.out.println("XML Result serializer...");
        PipedInputStream in = new PipedInputStream();
        PipedOutputStream out = new PipedOutputStream(in);
        CachedSerializer.XML.serialize(op, out);
        out.close();
        assertEquals(in, "<?xml version='1.0' encoding='UTF-8'?>"+expected);
        System.out.println();
    }

    protected void assertHtmlResult(Relationship op, String expected) throws IOException, InterruptedException {
        assertHtmlResult(op, expected, true);
    }

    protected void assertHtmlResult(Relationship op, String expected, boolean messagers) throws IOException {
        assertNotNull(op);

        if (messagers) System.out.println("HTML result serializer...");
        String result = CachedSerializer.HTML.serialize(op);
        if (messagers) System.out.println(result);
        Assert.assertEquals("", expected, result);

        if (messagers) System.out.println();
    }

    protected void assertStringResult(String op, String expected) throws IOException, InterruptedException {
    	assertStringResult(new AnimoExpression(op), expected, true);
    }

    protected void assertStringResult(Relationship op, String expected) throws IOException, InterruptedException {
    	assertStringResult(op, expected, true);
    }

    protected void assertStringResult(Relationship op, String expected, boolean messagers) throws IOException, InterruptedException {
        assertNotNull(op);

        if (messagers) System.out.println("VALUE result serializer...");
        String result = CachedSerializer.STRING.serialize(op);
        if (messagers) System.out.println(result);
        Assert.assertEquals("", expected, result);

        if (messagers) System.out.println();
    }

    protected void assertBinary(Relationship op, String expected) throws IOException {
        assertNotNull(op);
        System.out.println("Binary serializer...");
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        BinarySerializer._.serialize(op, out);
        String bin = out.toString();
        Assert.assertEquals("", expected, bin);
        System.out.println(bin);
        System.out.println();
    }

    private void deleteDir(File dir) {
        if (dir.isDirectory()) {
            String[] children = dir.list();
            for (String aChildren : children) {
                deleteDir(new File(dir, aChildren));
            }
        }
    }

    @Before
    public void start() {
        deleteDir(new File(DATA_FOLDER));
        startDB(DATA_FOLDER);
    }

    @After
    public void stop() {
    	shutdownDB();
    }

}