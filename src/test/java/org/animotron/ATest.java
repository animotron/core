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

import com.ctc.wstx.stax.WstxOutputFactory;
import junit.framework.Assert;
import org.animotron.exception.AnimoException;
import org.animotron.graph.GraphOperation;
import org.animotron.graph.serializer.*;
import org.animotron.manipulator.Evaluator;
import org.animotron.manipulator.PFlow;
import org.junit.After;
import org.junit.Before;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.index.IndexManager;

import java.io.*;

import static org.animotron.graph.AnimoGraph.*;
import static org.junit.Assert.assertNotNull;


/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public abstract class ATest {

    public static final String DATA_FOLDER = "data-test";
	
	public static final WstxOutputFactory OUTPUT_FACTORY = new WstxOutputFactory();

	protected void toConsole(PFlow ch) throws IOException {
		//XXX: code
//		if (instream == null) return;
//		
//		Object n; 
//		while ((n = instream.read()) != null) {
//			System.out.print(n.toString());
//		} 
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
        String result = AnimoSerializer.serialize(op, pretty);
        System.out.println(result);
        Assert.assertEquals("", expected, result);

        System.out.println();
    }

    protected void assertAnimoResult(Relationship op, String expected) throws IOException {
        assertAnimoResult(op, expected, false);
    }

    protected void assertAnimoResult(Relationship op, String expected, boolean pretty) throws IOException {
        assertNotNull(op);

        System.out.println("Animo result serializer...");
        String result = AnimoResultSerializer.serialize(op, pretty);
        System.out.println(result);
        Assert.assertEquals("", expected, result);

        System.out.println();
    }

    protected void assertXMLResult(Relationship op, String expected) throws IOException {
        assertNotNull(op);

        System.out.println("XML Result serializer...");

        PipedInputStream in = new PipedInputStream();
        PipedOutputStream out = new PipedOutputStream(in);

        XMLResultSerializer.serialize(op, out);
        out.close();
        assertEquals(in, "<?xml version='1.0' encoding='UTF-8'?>"+expected);
        System.out.println();
    }

    protected void assertStringResult(Relationship op, String expected) throws IOException, InterruptedException {
        assertNotNull(op);

        System.out.println("VALUE result serializer...");
        String result = StringResultSerializer.serialize(new PFlow(Evaluator._), op);
        System.out.println(result);
        Assert.assertEquals("", expected, result);

        System.out.println();
    }

    protected void assertBinary(Relationship op, String expected) throws IOException {
        assertNotNull(op);
        System.out.println("Binary serializer...");
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        BinarySerializer.serialize(op, out);
        String bin = out.toString();
        Assert.assertEquals("", expected, bin);
        System.out.println(bin);
        System.out.println();
    }

    public void cleanDb() {
        execute(new GraphOperation<Void>() {
            @Override
            public Void execute() throws AnimoException {
                removeNodes();
                return null;
            }
        });
    }

    private void removeNodes() {
        Node refNode = getROOT();
        for (Node node : getDb().getAllNodes()) {
        	boolean delete = true;
            for (Relationship rel : node.getRelationships()) {
            	if (rel.getStartNode().equals(refNode)) {
            		delete = false;
                } else {
                    clearIndex(rel);
            		rel.delete();
                }
            }
            if (delete && !refNode.equals(node)) {
                clearIndex(node);
                node.delete();
            }
        }
    }

    private void clearIndex(Node node) {
        IndexManager indexManager = getDb().index();
        for (String ix : indexManager.nodeIndexNames()) {
            indexManager.forNodes(ix).remove(node);
        }
    }

    private void clearIndex(Relationship relationship) {
        IndexManager indexManager = getDb().index();
        for (String ix : indexManager.relationshipIndexNames()) {
            indexManager.forRelationships(ix).remove(relationship);
        }
    }

    @Before
    public void setup() {
    	//cleanDb();
        start();
    }

    @After
    public void cleanup() {
        stop();
    }

    public static boolean deleteDir(File dir) {
        if (dir.isDirectory()) {
            String[] children = dir.list();
            for (String aChildren : children) {
                boolean success = deleteDir(new File(dir, aChildren));
                if (!success) {
                    return false;
                }
            }
        }
        return dir.delete();
    }
    
    //@BeforeClass
    public static void start() {
    	deleteDir(new File(DATA_FOLDER));
        startDB(DATA_FOLDER);
    }

    //@AfterClass
    public static void stop() {
    	shutdownDB();
    }
}