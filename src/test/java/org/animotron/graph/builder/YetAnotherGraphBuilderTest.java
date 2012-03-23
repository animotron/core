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
import org.animotron.expression.AnimoExpression;
import org.animotron.expression.Expression;
import org.animotron.expression.JExpression;
import org.animotron.expression.StAXExpression;
import org.animotron.graph.serializer.CachedSerializer;
import org.animotron.statement.operator.THE;
import org.junit.Test;

import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;
import java.io.StringReader;

import static org.animotron.graph.AnimoGraph.startDB;
import static org.animotron.graph.Properties.HASH;
import static org.animotron.expression.JExpression._;

/**
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class YetAnotherGraphBuilderTest extends ATest {
    
    private final XMLInputFactory FACTORY = new WstxInputFactory();

    private XMLStreamReader r(String xml) throws XMLStreamException {
        return FACTORY.createXMLStreamReader(new StringReader(xml));
    }

    private Expression the (Expression e) {
        return new JExpression(
            _(THE._, _(e))
        );
    }


    private void test_0(String animo, String xml) throws Throwable {
        Expression e;
        e = the(new AnimoExpression(new FastGraphBuilder(), animo));
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

    private void test_1(String animo, String xml) throws Throwable {
        Expression e;
        e = the(new AnimoExpression(new FastGraphBuilder(), animo));
        String inA = CachedSerializer.ANIMO.serialize(e);
        byte[] inH = (byte[]) HASH.get(e);
        e = new StAXExpression(new StreamGraphBuilder(), r(xml));
        String outA = CachedSerializer.ANIMO.serialize(e);
        byte[] outH = (byte[]) HASH.get(e);
        assertEquals(inH, outH);
        Assert.assertEquals(inA, outA);
    }

    private void test_2(String animo, String xml) throws Throwable {
        Expression e;
        e = the(new AnimoExpression(new StreamGraphBuilder(), animo));
        String outA = CachedSerializer.ANIMO.serialize(e);
        byte[] outH = (byte[]) HASH.get(e);
        e = new StAXExpression(new FastGraphBuilder(), r(xml));
        String inA = CachedSerializer.ANIMO.serialize(e);
        byte[] inH = (byte[]) HASH.get(e);
        assertEquals(inH, outH);
        Assert.assertEquals(inA, outA);
    }

    private void test(String animo, String xml) throws Throwable {
        test_0(animo, xml);
        cleanDB();
        startDB(DATA_FOLDER);
        test_1(animo, xml);
        cleanDB();
        startDB(DATA_FOLDER);
        test_2(animo, xml);
    }

    @Test
	public void test_00() throws Throwable {
        test("\\a", "<a/>");
	}

    @Test
	public void test_01() throws Throwable {
        test("\\x:a $x \"x-namespace\"", "<x:a xmlns:x=\"x-namespace\"/>");
	}

    @Test
	public void test_02() throws Throwable {
        test("\\a $ \"x-namespace\"", "<a xmlns=\"x-namespace\"/>");
	}

    @Test
	public void test_03() throws Throwable {
        test("\\a @b \"c\"", "<a b=\"c\"/>");
	}

    @Test
	public void test_04() throws Throwable {
        test("((??stylesheet \"path\") (\\a))", "<?stylesheet \"path\"?><a/>");
	}
}