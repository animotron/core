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
import org.animotron.ATest;
import org.animotron.expression.AnimoExpression;
import org.animotron.expression.Expression;
import org.animotron.expression.JExpression;
import org.animotron.expression.StAXExpression;
import org.animotron.graph.serializer.CachedSerializer;
import org.animotron.graph.serializer.DigestSerializer;
import org.animotron.statement.ml.ELEMENT;
import org.animotron.statement.operator.AN;
import org.animotron.statement.operator.THE;
import org.animotron.statement.query.ANY;
import org.animotron.statement.query.GETALL;
import org.junit.Assert;
import org.junit.Ignore;
import org.junit.Test;
import org.neo4j.graphdb.Direction;

import javax.xml.stream.XMLInputFactory;
import java.io.StringReader;

import static org.animotron.Properties.HASH;
import static org.animotron.expression.JExpression._;
import static org.animotron.expression.JExpression.element;


/**
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class BindTest extends ATest {

    public void test(Expression e, String animo) throws Exception {
        String inA = CachedSerializer.ANIMO.serialize(e);
        byte[] inH = (byte[]) HASH.get(e);
        assertEquals(inH, DigestSerializer._.serialize(e));
        cleanDb();
        Expression x = new AnimoExpression(animo);
        String outA = CachedSerializer.ANIMO.serialize(x);
        byte[] outH = (byte[]) HASH.get(x);
        assertEquals(outH, DigestSerializer._.serialize(x));
        assertEquals(inH, outH);
        Assert.assertEquals(inA, outA);
    }

    @Test
	public void test_00() throws Exception {
        Expression a = new JExpression(
            _(AN._, "a")
        );
        Expression b = new JExpression(new StreamGraphBuilder(),
            _(THE._, "b", _(a))
        );
        test(b, "the b a.");
	}

    @Test
	public void test_01() throws Exception {
        Expression a = new JExpression(
            _(AN._, "a")
        );
        Expression b = new JExpression(
            _(THE._, "b", _(a))
        );
        test(b, "the b a.");
	}

    @Test
	public void test_02() throws Exception {
        Expression a = new JExpression(
            _(AN._, "a")
        );
        Expression b = new JExpression(new StreamGraphBuilder(),
            _(THE._, "b", _(AN._, "x"), _(a))
        );
        test(b, "the b (x) (a).");
	}

    @Test
	public void test_03() throws Exception {
        Expression a = new JExpression(
            _(AN._, "a")
        );
        Expression b = new JExpression(
            _(THE._, "b", _(AN._, "x"), _(a))
        );
        test(b, "the b (x) (a).");
	}

    @Test
	public void test_04() throws Exception {
        Expression x = new JExpression(
            _(THE._, "x")
        );
        Expression y = new JExpression(
            _(THE._, "y")
        );
        Expression b = new JExpression(
            _(THE._, "b", _(AN._, x), _(AN._, y))
        );
        test(b, "the b (x) (y).");
	}

    @Test
	public void test_05() throws Exception {
        Expression a = new JExpression(
            _(ANY._, "a")
        );
        Expression b = new JExpression(
            _(THE._, "b", _(AN._, "y", _(a)))
        );
        test(b, "the b y any a.");
	}

    @Test
	public void test_06() throws Exception {
        Expression a = new JExpression(
            _(ANY._, "a")
        );
        Expression x = new JExpression(
            _(THE._, "x")
        );
        Expression y = new JExpression(
            _(THE._, "y")
        );
        Expression b = new JExpression(
            _(THE._, "b", _(AN._, x), _(AN._, y, _(a)))
        );
        test(b, "the b (x) (y any a).");
	}

    @Test
    @Ignore
	public void test_07() throws Exception {
        Expression x = new JExpression(
            _(THE._, "x")
        );
        Expression b = new JExpression(
            _(THE._, "y", _(x))
        );
        test(b, "the x y.");
	}

    private static final XMLInputFactory FACTORY = new WstxInputFactory();

    @Test
	public void test_08() throws Exception {
        Expression a = new AnimoExpression("any a.");
        Expression b = new AnimoExpression("all b.");
        Expression c = new AnimoExpression("the c bla bla bla.");
        Expression x = new JExpression(
            _(AN._, c, _(a), _(b))
        );
        Expression y = new StAXExpression(FACTORY.createXMLStreamReader(new StringReader("<y z=\"test\">content</y>")));
        Expression z = new JExpression(
            element("z", _(GETALL._, "e", _(x)), _(y.getEndNode().getSingleRelationship(ELEMENT._, Direction.OUTGOING)))
        );
        test(z, "\\z (get e c (any a) (all b)) (\\y (@z \"test\") \"content\").");
	}

}
