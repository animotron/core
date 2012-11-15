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
package org.animotron.graph.serializer;

import org.animotron.ATest;
import org.animotron.expression.AnimoExpression;
import org.animotron.expression.JExpression;
import org.animotron.statement.operator.AN;
import org.animotron.statement.operator.DEF;
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
    public void test_00() throws Throwable {
    	test("def a \\b", "a \\b.");
    }

    @Test
    public void test_01() throws Throwable {
    	test("def a \\ get element-name element-name \"b\"", "a \\ \"b\".");
    }

    @Test
    public void test_02() throws Throwable {
        __(new JExpression(_(DEF._, "b", value("c"))));
        test("def a \\ b", "a \\ b \"c\".");
    }

    @Test
    public void test_03() throws Throwable {
        __(new JExpression(_(DEF._, "b", value("c"))));
        test("def a \\ an b", "a \\ b \"c\".");
    }

    @Test
	public void test_04() throws Throwable {
    	test("def a \\ \"b\"", "a \\ \"b\".");
	}

    @Test
	public void test_05() throws Throwable {
    	test("def a \\b @c \"d\"", "a \\b @c \"d\".");
	}

    @Test
    public void test_06() throws Throwable {
    	test("def a \\b (@c \"d\") (\"e\")", "a \\b (@c \"d\") \"e\".");
    }

    @Test
    public void test_07() throws Throwable {
        __(
                new JExpression(_(DEF._, "b", value("b"))),
                new JExpression(_(DEF._, "c", value("c"))),
                new JExpression(_(DEF._, "d", value("d"))),
                new JExpression(_(DEF._, "e", value("e")))
        );
        test("def a \\ (b) (@ (c) (d)) (e)", "a \\ (b \"b\") (@ (c \"c\") (d \"d\")) (e \"e\").");
    }

    @Test
    public void test_08() throws Throwable {
        __(
                new JExpression(_(DEF._, "b", value("b"))),
                new JExpression(_(DEF._, "c", value("c"))),
                new JExpression(_(DEF._, "d", value("d"))),
                new JExpression(_(DEF._, "e", value("e")))
        );
        test("def a \\((b) (@ (c) (d)) (e))", "a \\ ((b \"b\") (@ (c \"c\") (d \"d\")) (e \"e\")).");
    }

    @Test
    public void test_09() throws Throwable {
        __(
                new JExpression(_(DEF._, "b", value("b"))),
                new JExpression(_(DEF._, "c", value("c"))),
                new JExpression(_(DEF._, "d", value("d"))),
                new JExpression(_(DEF._, "e", element("e", _(AN._, "b"), _(AN._, "c"), _(AN._, "d"))))
        );
        test("def a \\(b) (@ (c) (d)) (e)", "a \\ (b \"b\") (@ (c \"c\") (d \"d\")) (e \\ e (b \"b\") (c \"c\") (d \"d\")).");
    }
}