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
package org.animotron.graph.builder;

import com.ctc.wstx.stax.WstxInputFactory;
import junit.framework.Assert;
import org.animotron.ATest;
import org.animotron.expression.AnimoExpression;
import org.animotron.expression.AbstractExpression;
import org.animotron.expression.StAXExpression;
import org.animotron.graph.serializer.AnimoSerializer;
import org.junit.Ignore;
import org.junit.Test;

import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;
import java.io.StringReader;

import static org.animotron.Properties.HASH;
import static org.animotron.graph.Cache.key;


/**
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class YetAnotherGraphBuilderTest extends ATest {
    
    private final XMLInputFactory FACTORY = new WstxInputFactory();

    private XMLStreamReader r(String xml) throws XMLStreamException {
        return FACTORY.createXMLStreamReader(new StringReader(xml));
    }

    private void test_0(String animo, String xml) throws Exception {
        AbstractExpression e;
        e = new AnimoExpression(new FastGraphBuilder(), animo);
        String inA = AnimoSerializer.serialize(e);
        String inH = (String) key(HASH.get(e));
        cleanDb();
        e = new StAXExpression(new StreamGraphBuilder(), r(xml));
        String outA = AnimoSerializer.serialize(e);
        String outH = (String) key(HASH.get(e));
        Assert.assertEquals(inH, outH);
        Assert.assertEquals(inA, outA);
    }

    private void test_1(String animo, String xml) throws Exception {
        AbstractExpression e;
        e = new AnimoExpression(new FastGraphBuilder(), animo);
        String inA = AnimoSerializer.serialize(e);
        String inH = (String) key(HASH.get(e));
        e = new StAXExpression(new StreamGraphBuilder(), r(xml));
        String outA = AnimoSerializer.serialize(e);
        String outH = (String) key(HASH.get(e));
        Assert.assertEquals(inH, outH);
        Assert.assertEquals(inA, outA);
    }

    private void test_2(String animo, String xml) throws Exception {
        AbstractExpression e;
        e = new AnimoExpression(new StreamGraphBuilder(), animo);
        String outA = AnimoSerializer.serialize(e);
        String outH = (String) key(HASH.get(e));
        e = new StAXExpression(new FastGraphBuilder(), r(xml));
        String inA = AnimoSerializer.serialize(e);
        String inH = (String) key(HASH.get(e));
        Assert.assertEquals(inH, outH);
        Assert.assertEquals(inA, outA);
    }

    private void test(String animo, String xml) throws Exception {
        test_0(animo, xml);
        cleanDb();
        test_1(animo, xml);
        cleanDb();
        test_2(animo, xml);
    }

    @Test
	public void test_00() throws Exception {
        test("\\a", "<a/>");
	}

    @Test
	public void test_01() throws Exception {
        test("\\x:a $x \"x-namespace\"", "<x:a xmlns:x=\"x-namespace\"/>");
	}

    @Test
	public void test_02() throws Exception {
        test("\\a $ \"x-namespace\"", "<a xmlns=\"x-namespace\"/>");
	}

    @Test
	public void test_03() throws Exception {
        test("\\a @b \"c\"", "<a b=\"c\"/>");
	}

    @Test
    @Ignore
	public void test_04() throws Exception {
        test("(??stylesheet \"path\") (\\a)", "<?stylesheet=\"path\"?><a/>");
	}

}
