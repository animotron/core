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
package org.animotron.statement.math;

import org.animotron.ATest;
import org.animotron.expression.AnimoExpression;
import org.animotron.expression.Expression;
import org.animotron.expression.JExpression;
import org.junit.Test;

import static org.animotron.expression.AnimoExpression.__;
import static org.animotron.expression.JExpression._;
import static org.animotron.expression.JExpression.value;

/**
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class MathTest extends ATest {

	public void test_performace() throws Exception {
        Expression e = new JExpression(
            _(SUM._, value(1), value(2), value(3), value(4))
        );
        
        long time = System.currentTimeMillis();
        for (int i = 0; i<10000;i++) {
            time = System.currentTimeMillis();
        	assertStringResult(e, "10", false);
        	System.out.println(System.currentTimeMillis() - time);
        }
	}
	@Test
	public void test_00() throws Exception {
        Expression e = new JExpression(
            _(SUM._, value(1), value(2), value(3), value(4))
        );
    	assertStringResult(e, "10");
	}

    @Test
	public void test_01() throws Exception {
        Expression e = new JExpression(
            _(MUL._, value(2), value(2))
        );
    	assertStringResult(e, "4");
	}

    @Test
	public void test_02() throws Exception {
        Expression e = new JExpression(
            _(MUL._, value(2), value(2.0))
        );
    	assertStringResult(e, "4.0");
	}

    @Test
	public void test_03() throws Exception {
        Expression e = new JExpression(
            _(MUL._, value(2.0), value(2))
        );
    	assertStringResult(e, "4.0");
	}

    @Test
	public void test_04() throws Exception {
        Expression e = new JExpression(
            _(MUL._, value(2.0), value(2.0))
        );
    	assertStringResult(e, "4.0");
	}

    @Test
	public void test_05() throws Exception {
        Expression e = new JExpression(
            _(DIV._, value(4.0), value(2.0))
        );
    	assertStringResult(e, "2.0");
	}

    @Test
	public void test_06() throws Exception {
        Expression e = new JExpression(
            _(DIV._, value(4), value(2))
        );
    	assertStringResult(e, "2.0");
	}

    @Test
	public void test_07() throws Exception {
        Expression e = new JExpression(
            _(SUB._, value(1), value(2), value(3.0), value(4))
        );
    	assertStringResult(e, "-8.0");
	}

    @Test
	public void test_08() throws Exception {
        Expression e = new JExpression(
            _(SUM._, value("1"), value("2"), value("3.0"), value("4"))
        );
    	assertStringResult(e, "10.0");
	}
    
    @Test
	public void test_09() throws Exception {
    	Expression test = new AnimoExpression("- 5 * 6 7"); //5-6*7
    	assertStringResult(test, "-37");
    }

    @Test
	public void test_10() throws Exception {
        Expression e = new AnimoExpression("+ 1 2 3 4");
    	assertStringResult(e, "10");
	}

    @Test
	public void test_11() throws Exception {
        Expression e = new AnimoExpression("* 2 2");
    	assertStringResult(e, "4");
	}

    @Test
	public void test_12() throws Exception {
        Expression e = new AnimoExpression("* 2 2.0");
        
    	assertAnimo(e, "* 2 2.0.");
    	assertStringResult(e, "4.0");
	}

    @Test
	public void test_13() throws Exception {
        Expression e = new AnimoExpression("* 2.0 2");

    	assertAnimo(e, "* 2.0 2.");
        assertStringResult(e, "4.0");
	}

    @Test
	public void test_14() throws Exception {
        Expression e = new AnimoExpression("* 2.0 2.0");

        assertAnimo(e, "* 2.0 2.0.");
    	assertStringResult(e, "4.0");
	}

    @Test
	public void test_15() throws Exception {
        Expression e = new AnimoExpression("/ 4.0 2.0");

        assertAnimo(e, "/ 4.0 2.0.");
    	assertStringResult(e, "2.0");
	}

    @Test
	public void test_16() throws Exception {
    	assertStringResult("/ 4 2", "2.0");
	}

    @Test
	public void test_17() throws Exception {
        Expression e = new AnimoExpression("- 1 2 3.0 4");

        assertAnimo(e, "- 1 2 3.0 4.");
        assertStringResult(e, "-8.0");
	}

    @Test
	public void test_18() throws Exception {
        __(
            "the a (x 1) (y 2) (z 3)"
        );
    	assertStringResult("+ (get x a) (get y a) (get z a)", "6");
	}

    @Test
	public void test_19() throws Exception {
        __(
            "the a (x 1) (y 2) (z 3)",
            "the b + (get x) (get y) (get z)"
        );
    	assertStringResult("b a", "6");
	}

    @Test
	public void test_20() throws Exception {
    	assertStringResult("+ \"1\" \"2\" \"3\" \"4\"", "10");
	}

    @Test
	public void test_21() throws Exception {
    	assertStringResult("+ 1 \"2\" \"3.0\" 4", "10.0");
	}

    @Test
	public void test_22() throws Exception {
    	assertStringResult("+ -1", "-1");
	}

    @Test
	public void test_23() throws Exception {
    	assertStringResult("- -1", "1");
	}

    @Test
	public void test_24() throws Exception {
    	assertStringResult("/ 2", "0.5");
	}

    @Test
	public void test_25() throws Exception {
    	assertStringResult("/  5 2", "2.5");
	}

    @Test
	public void test_30() throws Exception {
        __(
            "the a 1",
            "the b 2",
            "the c 3",
            "the d 4"
        );
    	assertStringResult("a", "1");
    	assertStringResult("+ (a) (b) (c) (d)", "10");
	}

    @Test
	public void test_31() throws Exception {
        __(
            "the a (z) (1)",
            "the b (z) (2)",
            "the c (z) (3)",
            "the d (z) (4)"
        );
    	assertStringResult("+ all z", "10");
	}

    @Test
	public void test_32() throws Exception {
        __(
            "the a x 1",
            "the b x 2",
            "the c x 3",
            "the d x 4"
        );
    	assertStringResult("+ get x (a) (b) (c) (d)", "10");
	}

    @Test
	public void test_33() throws Exception {
        __(
            "the a (z) (x 1)",
            "the b (z) (x 2)",
            "the c (z) (x 3)",
            "the d (z) (x 4)"
        );
    	assertStringResult("+ get x all z", "10");
	}
}