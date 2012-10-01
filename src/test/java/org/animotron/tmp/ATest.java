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
package org.animotron.tmp;

import com.ctc.wstx.stax.WstxOutputFactory;
import junit.framework.Assert;
import org.animotron.expression.AnimoExpression;
import org.animotron.graph.serializer.CachedSerializer;
import org.animotron.graph.serializer.DigestSerializer;
import org.animotron.io.PipedInput;
import org.animotron.io.PipedOutput;
import org.animotron.statement.value.VALUE;
import org.animotron.utils.MessageDigester;
import org.junit.After;
import org.junit.Before;
import org.neo4j.graphdb.Relationship;

import java.io.*;

import static org.animotron.expression.AnimoExpression.__;
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

	protected String uuid() {
		return MessageDigester.uuid().toString();
	}
	
	protected void sleep(int secs) {
		VALUE._.waitToBeEmpty();

		try {
			Thread.sleep(secs * 1000);
		} catch (Exception e) {}
	}
	
	protected String _(String name, String ... types) {
		
//		String uuid = uuid();
		String uuid = name;
		
		StringBuilder sb = new StringBuilder();
		sb.append("def ").append(uuid);
		
		for (int i = 0; i < types.length; i++) {
			sb.append(" (").append(types[i]).append(")");
		}
		
		sb.append(" '").append(name).append("'.");
		
//		System.out.println(sb.toString());
		
		__(sb.toString());

        // deadlock workaround
        //try {Thread.sleep(250);} catch (InterruptedException e) {}

        return uuid;
	}
	
	protected void testAnimiParser(String msg, String expected) throws IOException {
		PipedOutput<Relationship> op = new PipedOutput<Relationship>();
		PipedInput<Relationship> ip = op.getInputStream();
		
		Reader reader = new StringReader(msg);
		Dialogue dlg = new Dialogue(reader, op);
		(new Thread(dlg)).run();
		
		Relationship result = null;
		for (Relationship r : ip) {
			if (result == null)
				result = r;
			else
				Assert.fail("more then one result");
		}
		
		if (result == null)
			Assert.fail("expecting animo object '"+expected+"', but get none");
		
		String actual = CachedSerializer.ANIMO.serialize(result);
		Assert.assertEquals(expected, actual);

		
		reader.close();
	}

	protected void testAnimi(String msg, String expected) throws IOException {

		PipedOutputStream output = new PipedOutputStream();
		PipedInputStream input = new PipedInputStream(output);
		
		Reader reader = new StringReader(msg);
		Dialogue dlg = new Dialogue(new StringReader(msg), output);
		(new Thread(dlg)).run();
		
		assertEquals(input, expected);
		
		reader.close();
	}

	protected AnimoExpression testAnimo(String exp) throws Throwable {
        return testAnimo(exp, exp);
    }

	protected AnimoExpression testAnimo(String in, String out) throws Throwable {
        AnimoExpression expression = new AnimoExpression(in);
        assertEquals((byte[]) HASH.get(expression), DigestSerializer._.serialize(expression));
        assertAnimo(expression, out);
        
        return expression;
    }

	protected void testAnimoResult(String exp, String res) throws Throwable {
		testAnimoResult(exp, exp, res);
    }

	protected void testAnimoResult(String in, String out, String res) throws Throwable {
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

    private void assertEquals(byte[] a, byte[] b) {
        Assert.assertEquals(a.length, b.length);
        for (int i = 0; i < a.length; i++)
            Assert.assertEquals(a[i], b[i]);
    }

	private void assertEquals(InputStream stream, String expecteds) throws IOException {
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

    private void deleteDir(File dir) {
        if (dir.isDirectory()) {
            for (String aChildren : dir.list()) {
                deleteDir(new File(dir, aChildren));
            }
        }
        dir.delete();
    }

    public void cleanDB() {
        shutdownDB();
        deleteDir(new File(DATA_FOLDER));
    }

    @Before
    public void start() {
        cleanDB();
        startDB(DATA_FOLDER);
    }

    @After
    public void stop() {
        shutdownDB();
    }

}