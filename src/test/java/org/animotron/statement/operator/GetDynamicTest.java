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
import org.animotron.expression.JExpression;
import org.animotron.statement.query.ALL;
import org.animotron.statement.query.GET;
import org.junit.Test;

import static org.animotron.expression.JExpression.*;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class GetDynamicTest extends ATest {
	
	@Test
	public void getOnManyAN() throws Throwable {

        __(
            new JExpression(
                _(DEF._, "A", _(AN._, "Z", value("A")))
            ),
            new JExpression(
                _(DEF._, "B", _(AN._, "Z", value("B")))
            )
        );

    	JExpression d = new JExpression(
			_(DEF._, "d", _(GET._, _(AN._, "Z"), _(AN._, "A"), _(AN._, "B")))
		);
        assertAnimoResult(d, "d", " \"A\"", " \"B\"", ".");
	}

	@Test
	public void getOnManyANbyIS() throws Throwable {

        __(
            new JExpression(
                _(DEF._, "ZZ", _(AN._, "Z"))
            ),
            new JExpression(
                _(DEF._, "A", _(AN._, "Z", value("A")))
            ),
            new JExpression(
                _(DEF._, "B", _(AN._, "ZZ", value("B")))
            )
        );

    	JExpression d = new JExpression(
			_(DEF._, "d", _(GET._, _(AN._, "Z"), _(AN._, "A"), _(AN._, "B")))
		);
        assertAnimoResult(d, "d", " \"A\"", " \"B\"", ".");
	}

	@Test
    public void get_via_is() throws Throwable {

        __(
            new JExpression(
                _(DEF._, "B", _(AN._, "A"))
            ),
            new JExpression(
                _(DEF._, "C", _(AN._, "Z"), _(AN._, "B", value("π")))
            ),
            new JExpression(
                _(DEF._, "D", _(AN._, "Z"), _(AN._, "A", value("Aπ")))
            )
        );

        JExpression E = new JExpression(
            _(DEF._, "E", _(GET._, "A", _(ALL._, "Z")))
        );
        assertAnimoResult(E, "E \"π\" \"Aπ\".");

        JExpression E1 = new JExpression(
            _(DEF._, "E1", _(GET._, "B", _(ALL._, "Z")))
        );
        assertAnimoResult(E1, "E1 \"π\".");

        JExpression F = new JExpression(
            _(DEF._, "F", _(GET._, _(ALL._, "A"), _(ALL._, "Z")))
        );
        assertAnimoResult(F, "F \"π\".");
    }

    @Test
    public void test_00() throws Throwable {
    	testAnimo("def z a z1.");
    	testAnimo("def b z1 \"z1\".");

    	testAnimoResult("get (get a z) (b).", "\"z1\".");
    }

    @Test
    public void test_01() throws Throwable {
    	testAnimo("def z (k) (a z1).");
    	testAnimo("def b (k) (z1 \"z1\").");

    	testAnimoResult("get a z.", "z1.");
    	testAnimoResult("get (get a z) (all k).", "z1. \"z1\".");
    }

    @Test
    public void test_02() throws Throwable {
    	testAnimo("def z (k) (a z1).");
    	testAnimo("def b (k) (z1 \"z1\").");

    	testAnimoResult("get (get a z) (a).", "");
    }

    @Test
    public void test_03() throws Throwable {
    	testAnimo("def z (k) (a z1).");
    	testAnimo("def b (z) (z1 \"z1\").");
    	testAnimo("def c (z) (z1 \"2z1\").");

    	testAnimoResult("get (get a z) (all z).", "\"z1\". \"2z1\".");
    }

    @Test
    public void test_04() throws Throwable {
        testAnimo("def z a z1.");
        testAnimo("def x b z1.");

        testAnimoResult("get (get a z) (z).", "z1.");
        testAnimoResult("get (get a z) (x).", "z1.");
    }

    @Test
    public void test_05() throws Throwable {
        testAnimo("def x y z.");

        testAnimoResult("get y x.", "z.");
        testAnimoResult("get z x.", "z.");
//        testAnimoResult("all y.", "z.");
    }

    @Test
    public void test_06() throws Throwable {
        testAnimo("def y a.");
        testAnimo("def x y z.");

//        testAnimoResult("all a.", "z.");
        testAnimoResult("all a.", "y a.");
    }

    @Test
    public void test_07() throws Throwable {
        testAnimo("def y a.");
        testAnimo("def x y z.");

//        testAnimoResult("all a use y.", "z.");
        testAnimoResult("all a use y.", "y a.");
    }

    @Test
    public void test_08() throws Throwable {
        testAnimo("def y ^a.");
        testAnimo("def x y.");

        testAnimoResult("all a use y.", "y a. x y a.");
    }

    @Test
    public void test_09() throws Throwable {
        testAnimo("def a ^b.");
        testAnimo("def y a.");
        testAnimo("def x y z.");

//        testAnimoResult("all b use y.", "z.");
        testAnimoResult("all b use y.", "y a b.");
    }

    @Test
    public void test_10() throws Throwable {
        testAnimo("def a ^b.");
        testAnimo("def y ^a.");
        testAnimo("def x y.");

        testAnimoResult("all b use y.", "y a b. x y a b.");
    }

    @Test
    public void test_10_1() throws Throwable {
        testAnimo("def a b.");
        testAnimo("def y ^a.");
        testAnimo("def x y.");

        testAnimoResult("all b use y.", "");
    }

    @Test
    public void test_10_2() throws Throwable {
        testAnimo("def a ^b.");
        testAnimo("def y a.");
        testAnimo("def x y.");

        testAnimoResult("all b use y.", "y a b.");
    }

    @Test
    public void test_11() throws Throwable {
        testAnimo("def y a.");
        testAnimo("def x y z.");

        testAnimoResult("prefer a.", "");
    }

    @Test
    public void test_12() throws Throwable {
        testAnimo("def y a.");
        testAnimo("def x y z.");

//        testAnimoResult("prefer a use y.", "z.");
        testAnimoResult("prefer a use y.", "y a.");
    }

    @Test
    public void test_13() throws Throwable {
        testAnimo("def y ^a.");
        testAnimo("def x y.");

        testAnimoResult("prefer a use y.", "y a. x y a.");
    }

    @Test
    public void test_14() throws Throwable {
        testAnimo("def a ^b.");
        testAnimo("def y a.");
        testAnimo("def x y z.");

//        testAnimoResult("prefer b use y.", "z.");
        testAnimoResult("prefer b use y.", "y a b.");
    }

    @Test
    public void test_15() throws Throwable {
        testAnimo("def a ^b.");
        testAnimo("def y ^a.");
        testAnimo("def x y.");

        testAnimoResult("prefer b use y.", "y a b. x y a b.");
    }

    @Test
    public void test_15_1() throws Throwable {
        testAnimo("def a ^b.");
        testAnimo("def y a.");
        testAnimo("def x y.");

        testAnimoResult("prefer b use y.", "y a b.");
    }

    @Test
    public void test_15_2() throws Throwable {
        testAnimo("def a b.");
        testAnimo("def y ^a.");
        testAnimo("def x y.");

        testAnimoResult("prefer b use y.", "");
    }

    @Test
    public void test_16() throws Throwable {
        testAnimo("def a b c.");
        testAnimo("def x a.");

        testAnimoResult("get b x.", "c.");
    }

    @Test
    public void test_17() throws Throwable {
        testAnimo("def a b (c) (d) (e).");
        assertAnimoResult("an get b a", "c. d. e.");
    }

    @Test
    public void test_18() throws Throwable {
        testAnimo("def x c.");
        testAnimo("def y d.");
        testAnimo("def z e.");
        testAnimo("def foo c, d, e.");
        testAnimo("def bar (c) (d) (e).");
        testAnimo("def a b (c) (d) (e).");
        assertAnimoResult("all get b a", "bar (c) (d) (e). foo (c) (d) (e).");
    }

    @Test
    public void test_19() throws Throwable {
        testAnimo("def x c, d, e.");
        testAnimo("def a b (c) (d) (e).");
        assertAnimoResult("any get b a", "x (c) (d) (e).");
    }

    @Test
    public void test_20() throws Throwable {
        testAnimo("def x (c) (d) (e).");
        testAnimo("def a b (c) (d) (e).");
        assertAnimoResult("any get b a", "x (c) (d) (e).");
    }

}