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
package org.animotron.graph.serializer;

import org.animotron.ATest;
import org.animotron.expression.AnimoExpression;
import org.animotron.expression.JExpression;
import org.animotron.statement.operator.AN;
import org.animotron.statement.operator.THE;
import org.junit.Assert;
import org.junit.Test;

import static org.animotron.Properties.HASH;
import static org.animotron.expression.JExpression.*;
import static org.animotron.graph.Cache.key;


/**
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class AnimoMLSerializerTest extends ATest {

    private void test(String in, String out) throws Exception {
        AnimoExpression expression = new AnimoExpression(in);
        Assert.assertEquals(HASH.get(expression), key(DigestSerializer.serialize(expression)));
        assertAnimoResult(expression, out);
    }

    @Test
    public void test_00() throws Exception {
        test("the a \\b", "the a \\b");
    }

    @Test
    public void test_01() throws Exception {
        test("the a \\ get element-name have element-name \"b\"", "the a \\ have element-name \"b\"");
    }

    @Test
    public void test_02() throws Exception {
        new JExpression(_(THE._, "b", text("c")));
        test("the a \\ b", "the a \\ the b \"c\"");
    }

    @Test
    public void test_03() throws Exception {
        new JExpression(_(THE._, "b", text("c")));
        test("the a \\ an b", "the a \\ the b \"c\"");
    }

    @Test
	public void test_04() throws Exception {
        test("the a \\ \"b\"", "the a \\b");
	}

    @Test
	public void test_05() throws Exception {
        test("the a \\b @c \"d\"", "the a \\b @c \"d\"");
	}

    @Test
    public void test_06() throws Exception {
        test("the a \\b (@c \"d\") (\"e\")", "the a \\b (@c \"d\") (\"e\")");
    }

    @Test
    public void test_07() throws Exception {
        new JExpression(_(THE._, "b", text("b")));
        new JExpression(_(THE._, "c", text("c")));
        new JExpression(_(THE._, "d", text("d")));
        new JExpression(_(THE._, "e", text("e")));
        test("the a \\ (b) (@ (c) (d)) (e)", "the a \\ (the b \"b\") (@ (the c \"c\") (the d \"d\")) (the e \"e\")");
    }

    @Test
    public void test_08() throws Exception {
        new JExpression(_(THE._, "b", text("b")));
        new JExpression(_(THE._, "c", text("c")));
        new JExpression(_(THE._, "d", text("d")));
        new JExpression(_(THE._, "e", text("e")));
        test("the a \\((b) (@ (c) (d)) (e))", "the a \\ ((the b \"b\") (@ (the c \"c\") (the d \"d\")) (the e \"e\"))");
    }

    @Test
    public void test_09() throws Exception {
        new JExpression(_(THE._, "b", text("b")));
        new JExpression(_(THE._, "c", text("c")));
        new JExpression(_(THE._, "d", text("d")));
        new JExpression(_(THE._, "e", element("e", _(AN._, "b"), _(AN._, "c"), _(AN._, "d"))));
        test("the a \\(b) (@ (c) (d)) (e)", "the a \\ (the b \"b\") (@ (the c \"c\") (the d \"d\")) (the e \\e (the b \"b\") (the c \"c\") (the d \"d\"))");
    }

}
