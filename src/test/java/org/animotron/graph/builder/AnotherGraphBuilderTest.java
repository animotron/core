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
package org.animotron.graph.builder;

import com.ctc.wstx.stax.WstxInputFactory;
import junit.framework.Assert;
import org.animotron.ATest;
import org.animotron.expression.StAXExpression;
import org.animotron.graph.serializer.CachedSerializer;
import org.junit.Ignore;
import org.junit.Test;

import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;
import java.io.StringReader;

import static org.animotron.graph.AnimoGraph.startDB;
import static org.animotron.graph.Properties.HASH;


/**
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class AnotherGraphBuilderTest extends ATest {
    
    private final XMLInputFactory FACTORY = new WstxInputFactory();

    private XMLStreamReader r(String xml) throws XMLStreamException {
        return FACTORY.createXMLStreamReader(new StringReader(xml));
    }

    private void test_0(String xml) throws Exception {
        StAXExpression e;
        e = new StAXExpression(new FastGraphBuilder(), r(xml));
        String inA = CachedSerializer.ANIMO.serialize(e);
        byte[] inH = (byte[]) HASH.get(e);
        cleanDB();
        startDB(DATA_FOLDER);
        e = new StAXExpression(new StreamGraphBuilder(), r(xml));
        String outA = CachedSerializer.ANIMO.serialize(e);
        byte[] outH = (byte[]) HASH.get(e);
        assertEquals(inH, outH);
        Assert.assertEquals(inA, outA);
    }

    private void test_1(String xml) throws Exception {
        StAXExpression e;
        e = new StAXExpression(new StreamGraphBuilder(), r(xml));
        String outA = CachedSerializer.ANIMO.serialize(e);
        byte[] outH = (byte[]) HASH.get(e);
        e = new StAXExpression(new FastGraphBuilder(), r(xml));
        String inA = CachedSerializer.ANIMO.serialize(e);
        byte[] inH = (byte[]) HASH.get(e);
        assertEquals(inH, outH);
        Assert.assertEquals(inA, outA);
    }

    private void test_2(String xml) throws Exception {
        StAXExpression e;
        e = new StAXExpression(new FastGraphBuilder(), r(xml));
        String inA = CachedSerializer.ANIMO.serialize(e);
        byte[] inH = (byte[]) HASH.get(e);
        e = new StAXExpression(new StreamGraphBuilder(), r(xml));
        String outA = CachedSerializer.ANIMO.serialize(e);
        byte[] outH = (byte[]) HASH.get(e);
        assertEquals(inH, outH);
        Assert.assertEquals(inA, outA);
    }

    private void test(String xml) throws Exception {
        test_0(xml);
        cleanDB();
        startDB(DATA_FOLDER);
        test_1(xml);
        cleanDB();
        startDB(DATA_FOLDER);
        test_2(xml);
    }

    @Test
	public void test_00() throws Exception {
        test("<a/>");
	}

    @Test
	public void test_01() throws Exception {
        test("<x:a xmlns:x=\"x-namespace\"/>");
	}

    @Test
	public void test_02() throws Exception {
        test("<a xmlns=\"x-namespace\"/>");
	}

    @Test
	public void test_03() throws Exception {
        test("<a b=\"c\"/>");
	}

    @Test
	public void test_04() throws Exception {
        test("<?stylesheet path=\"path\"?><a/>");
	}
}