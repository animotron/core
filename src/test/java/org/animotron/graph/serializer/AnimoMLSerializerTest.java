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
import org.junit.Test;

import java.io.IOException;

import static org.animotron.expression.JExpression.*;

/**
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class AnimoMLSerializerTest extends ATest {

    private void test (String src, String res) throws IOException {
        assertAnimoResult(new AnimoExpression(src), res);
    }

    @Test
    public void test_00() throws Exception {
    	test("the a \\b", "the a \\b.");
    }

    @Test
    public void test_01() throws Exception {
    	test("the a \\ get element-name element-name \"b\"", "the a \\ element-name \"b\".");
    }

    @Test
    public void test_02() throws Exception {
        __(new JExpression(_(THE._, "b", text("c"))));
        test("the a \\ b", "the a \\ b \"c\".");
    }

    @Test
    public void test_03() throws Exception {
        __(new JExpression(_(THE._, "b", text("c"))));
        test("the a \\ an b", "the a \\ b \"c\".");
    }

    @Test
	public void test_04() throws Exception {
    	test("the a \\ \"b\"", "the a \\ \"b\".");
	}

    @Test
	public void test_05() throws Exception {
    	test("the a \\b @c \"d\"", "the a \\b @c \"d\".");
	}

    @Test
    public void test_06() throws Exception {
    	test("the a \\b (@c \"d\") (\"e\")", "the a \\b (@c \"d\") \"e\".");
    }

    @Test
    public void test_07() throws Exception {
        __(
                new JExpression(_(THE._, "b", text("b"))),
                new JExpression(_(THE._, "c", text("c"))),
                new JExpression(_(THE._, "d", text("d"))),
                new JExpression(_(THE._, "e", text("e")))
        );
        test("the a \\ (b) (@ (c) (d)) (e)", "the a \\ (b \"b\") (@ (c \"c\") (d \"d\")) (e \"e\").");
    }

    @Test
    public void test_08() throws Exception {
        __(
                new JExpression(_(THE._, "b", text("b"))),
                new JExpression(_(THE._, "c", text("c"))),
                new JExpression(_(THE._, "d", text("d"))),
                new JExpression(_(THE._, "e", text("e")))
        );
        test("the a \\((b) (@ (c) (d)) (e))", "the a \\ ((b \"b\") (@ (c \"c\") (d \"d\")) (e \"e\")).");
    }

    @Test
    public void test_09() throws Exception {
        __(
                new JExpression(_(THE._, "b", text("b"))),
                new JExpression(_(THE._, "c", text("c"))),
                new JExpression(_(THE._, "d", text("d"))),
                new JExpression(_(THE._, "e", element("e", _(AN._, "b"), _(AN._, "c"), _(AN._, "d"))))
        );
        test("the a \\(b) (@ (c) (d)) (e)", "the a \\ (b \"b\") (@ (c \"c\") (d \"d\")) (e \\e (b \"b\") (c \"c\") (d \"d\")).");
    }
}