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
package org.animotron.statement.operator;

import org.animotron.ATest;
import org.animotron.expression.AnimoExpression;
import org.animotron.expression.Expression;
import org.junit.Ignore;
import org.junit.Test;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class EachTest extends ATest {

    @Test
    public void test() throws Throwable {
        __(
    		"def a (s) (x 1).",
    		"def b (s) (x 2).",
    		"def c (s) (x 3).",
    		"def foo get 1."
		);

        assertAnimoResult("each (all s) (foo get x)", "foo 1. foo 2. foo 3.");
    }

    @Test
    public void test_00() throws Throwable {
        __("def ul get 1.");

        Expression s = new AnimoExpression("ul each ((A) (B) (C)) (li)");
        assertAnimoResult(s, "ul (li) (li) (li).");
    }

    @Test
    @Ignore
    public void test_01() throws Throwable {

        Expression s = new AnimoExpression("x each ((a1) (b1) (c1)) ((a2) (b2) (c2)) ((a3) (b3) (c3))");

        assertAnimoResult(s,

                "x " +

                        "(c1 b1 a1) " +
                        "(c1 b1 a2) " +
                        "(c1 b1 a3) " +

                        "(c1 b2 a1) " +
                        "(c1 b2 a2) " +
                        "(c1 b2 a3) " +

                        "(c1 b3 a1) " +
                        "(c1 b3 a2) " +
                        "(c1 b3 a3) " +

                        "(c2 b1 a1) " +
                        "(c2 b1 a2) " +
                        "(c2 b1 a3) " +

                        "(c2 b2 a1) " +
                        "(c2 b2 a2) " +
                        "(c2 b2 a3) " +

                        "(c2 b3 a1) " +
                        "(c2 b3 a2) " +
                        "(c2 b3 a3) " +

                        "(c3 b1 a1) " +
                        "(c3 b1 a2) " +
                        "(c3 b1 a3) " +

                        "(c3 b2 a1) " +
                        "(c3 b2 a2) " +
                        "(c3 b2 a3) " +

                        "(c3 b3 a1) " +
                        "(c3 b3 a2) " +
                        "(c3 b3 a3)."
       );
    }

    @Test
    public void test_02() throws Throwable {
        __(
    		"def a x 1 2 3.",
    		"def foo get 1."
		);

        assertAnimoResult("each (get x a) (foo this x)", "foo 1. foo 2. foo 3.");
    }

    @Test
    public void test_03() throws Throwable {
        __(
    		"def a x 1 2 3.",
    		"def foo get 1."
		);

        assertAnimoResult("each (get y a) (foo this x)", "");
    }

}