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

import static org.animotron.graph.AnimoGraph.*;
import static org.junit.Assert.assertNotNull;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PipedInputStream;
import java.io.PipedOutputStream;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;

import junit.framework.Assert;

import org.animotron.graph.CommonBuilder;
import org.animotron.graph.GraphOperation;
import org.animotron.manipulator.PFlow;
import org.animotron.serializer.AnimoResultSerializer;
import org.animotron.serializer.StringResultSerializer;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.Transaction;
import org.neo4j.graphdb.index.IndexManager;

import com.ctc.wstx.stax.WstxOutputFactory;


/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public abstract class ATest {

    private static final String DATA_FOLDER = "data-test";
	
	public static final WstxOutputFactory OUTPUT_FACTORY = new WstxOutputFactory();

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

        "xmlns:math='animo/math' " +

		"xmlns:Q='animo/quantity' ";

	protected void store(final Map<String, String> nameDataMap) throws XMLStreamException {
		XMLStreamException e = 
			execute(new GraphOperation<XMLStreamException>() {
				@Override
				public XMLStreamException execute() {
			        for (Entry<String, String> entry : nameDataMap.entrySet()) {
			        	try {
							CommonBuilder.build(entry.getValue());
						} catch (XMLStreamException e) {
							return e;
						}
			        }
			        return null;
				}
			});
		if (e != null) throw e;
    }

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

	protected void assertAnimo(Relationship op, String expected) throws IOException, InterruptedException {
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
        assertEquals(in, "<?xml version='1.0' encoding='UTF-8'?>"+expected);
	}

	protected void assertString(Relationship op, String expected) throws IOException, InterruptedException {
        assertNotNull(op);

       System.out.println("String result serializer...");
        StringResultSerializer serializer = new StringResultSerializer();
        serializer.serialize(op);
        Assert.assertEquals(expected, serializer.getString());
	}
	
	//database cleaning (thanks to mh)
    public Map<String, Object> cleanDb() {
        return cleanDb(Long.MAX_VALUE);
    }
    
    public Map<String, Object> cleanDb(long maxNodesToDelete) {
        Map<String, Object> result = new HashMap<String, Object>();
        Transaction tx = beginTx();
        try {
            clearIndex(result);
            removeNodes(result,maxNodesToDelete);
            tx.success();
        } finally {
            finishTx(tx);
        }
        initDB();
        return result;
    }

    private void removeNodes(Map<String, Object> result, long maxNodesToDelete) {
        Node refNode = getROOT();
        long nodes = 0, relationships = 0;
        for (Node node : getDb().getAllNodes()) {
        	boolean delete = true;
            for (Relationship rel : node.getRelationships()) {
            	if (rel.getStartNode().equals(refNode))
            		delete = false;
            	else
            		rel.delete();
                relationships++;
            }
            if (delete && !refNode.equals(node)) {
                node.delete();
                nodes++;
            }
            if (nodes >= maxNodesToDelete) break;
        }
        result.put("maxNodesToDelete", maxNodesToDelete);
        result.put("nodes", nodes);
        result.put("relationships", relationships);

    }

    private void clearIndex(Map<String, Object> result) {
        IndexManager indexManager = getDb().index();
        result.put("node-indexes", Arrays.asList(indexManager.nodeIndexNames()));
        result.put("relationship-indexes", Arrays.asList(indexManager.relationshipIndexNames()));
        for (String ix : indexManager.nodeIndexNames()) {
            indexManager.forNodes(ix).delete();
        }
        for (String ix : indexManager.relationshipIndexNames()) {
            indexManager.forRelationships(ix).delete();
        }
    }	

    @Before
    public void setup() {
    	cleanDb();
    }

    @After
    public void cleanup() {
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
    
    @BeforeClass
    public static void start() {
    	deleteDir(new File(DATA_FOLDER));
    	startDB(DATA_FOLDER);
    }

    @AfterClass
    public static void stop() {
    	shutdownDB();
    }

}
