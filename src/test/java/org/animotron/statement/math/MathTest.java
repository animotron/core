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
package org.animotron.statement.math;

import org.animotron.ATest;
import org.animotron.expression.AnimoExpression;
import org.animotron.expression.Expression;
import org.junit.Ignore;
import org.junit.Test;

import static org.animotron.expression.AnimoExpression.__;

/**
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class MathTest extends ATest {

	public void test_performace() throws Throwable {
        Expression e = new AnimoExpression("+ 1 2 3 4");
        long time = System.currentTimeMillis();
        for (int i = 0; i<10000;i++) {
            time = System.currentTimeMillis();
        	assertStringResult(e, "10", false);
        	System.out.println(System.currentTimeMillis() - time);
        }

	}
	@Test
	public void test_00() throws Throwable {
        Expression e = new AnimoExpression("+ 1 2 3 4");
        assertStringResult(e, "10");
	}

    @Test
	public void test_01() throws Throwable {
        Expression e = new AnimoExpression("* 2 2");
        assertStringResult(e, "4");
	}

    @Test
	public void test_02() throws Throwable {
        Expression e = new AnimoExpression("* 2 2.0");
    	assertStringResult(e, "4.0");
	}

    @Test
	public void test_03() throws Throwable {
        Expression e = new AnimoExpression("* 2.0 2");
    	assertStringResult(e, "4.0");
	}

    @Test
	public void test_04() throws Throwable {
        Expression e = new AnimoExpression("* 2.0 2.0");
    	assertStringResult(e, "4.0");
	}

    @Test
	public void test_05() throws Throwable {
        Expression e = new AnimoExpression("/ 4.0 2.0");
    	assertStringResult(e, "2.0");
	}

    @Test
	public void test_06() throws Throwable {
        Expression e = new AnimoExpression("/ 4 2");
    	assertStringResult(e, "2");

        e = new AnimoExpression("/ 5 2");
    	assertStringResult(e, "2.5");
    }

    @Test
    @Ignore //ignore for now
	public void test_07() throws Throwable {
        Expression e = new AnimoExpression("- 1 2 3.0 4");
    	assertStringResult(e, "-8.0");
	}

    @Test
	public void test_08() throws Throwable {
        Expression e = new AnimoExpression("+ 1 2 3.0 4");
    	assertStringResult(e, "10.0");
	}
    
    @Test
    @Ignore //ignore for now
	public void test_09() throws Throwable {
    	assertStringResult("- 5 * 6 7", "-37"); //5-(6*7) = 5 - 42 = -37

    	assertStringResult("* 5 - 6 7", "-5"); //5*(6-7) = 5 *(-1) = -5
    }

    @Test
	public void test_10() throws Throwable {
    	assertStringResult("+ 1 2 3 4", "10");
	}

    @Test
	public void test_11() throws Throwable {
    	assertStringResult("* 2 2", "4");
	}

    @Test
	public void test_12() throws Throwable {
        Expression e = new AnimoExpression("* 2 2.0");
        
    	assertAnimo(e, "* 2 2.0.");
    	assertStringResult(e, "4.0");
	}

    @Test
	public void test_13() throws Throwable {
        Expression e = new AnimoExpression("* 2.0 2");

    	assertAnimo(e, "* 2.0 2.");
        assertStringResult(e, "4.0");
	}

    @Test
	public void test_14() throws Throwable {
        Expression e = new AnimoExpression("* 2.0 2.0");

        assertAnimo(e, "* 2.0 2.0.");
    	assertStringResult(e, "4.0");
	}

    @Test
	public void test_15() throws Throwable {
        Expression e = new AnimoExpression("/ 4.0 2.0");

        assertAnimo(e, "/ 4.0 2.0.");
    	assertStringResult(e, "2.0");
	}

    @Test
	public void test_16() throws Throwable {
    	assertStringResult("/ 4 2", "2");
	}

    @Test
    @Ignore //ignore for now
	public void test_17() throws Throwable {
        Expression e = new AnimoExpression("- 1 2 3.0 4");

        assertAnimo(e, "- 1 2 3.0 4.");
        assertStringResult(e, "-8.0");
	}

    @Test
	public void test_18() throws Throwable {
        __(
            "def a (x 1) (y 2) (z 3)"
        );
    	assertStringResult("+ (get x a) (get y a) (get z a)", "6");
	}

    @Test
	public void test_19() throws Throwable {
        __(
            "def a (x 1) (y 2) (z 3)",
            "def b + (get x) (get y) (get z)"
        );
    	assertStringResult("b ^a", "6");
	}

    @Test
	public void test_20() throws Throwable {
    	assertStringResult("+ \"1\" \"2\" \"3\" \"4\"", "10");
	}

    @Test
	public void test_21() throws Throwable {
    	assertStringResult("+ 1 \"2\" \"3.0\" 4", "10.0");
	}

    @Test
	public void test_22() throws Throwable {
    	assertStringResult("+ -1", "-1");
	}

    @Test
	public void test_23() throws Throwable {
    	assertStringResult("- -1", "1");
	}

    @Test
	public void test_24() throws Throwable {
    	assertStringResult("/ 2", "0.5");
	}

    @Test
	public void test_25() throws Throwable {
    	assertStringResult("/  5 2", "2.5");
	}

    @Test
    @Ignore //TODO: deadlocking
	public void test_30() throws Throwable {
        __(
            "def a 1",
            "def b 2",
            "def c 3",
            "def d 4"
        );
    	assertStringResult("a", "1");
    	assertStringResult("+ (a) (b) (c) (d)", "10");
	}

    @Test
    @Ignore //not sure that this one correct
	public void test_31() throws Throwable {
        __(
            "def a (z) (1)",
            "def b (z) (2)",
            "def c (z) (3)",
            "def d (z) (4)"
        );
    	assertStringResult("+ all z", "10");
	}

    @Test
	public void test_32() throws Throwable {
        __(
            "def a x 1",
            "def b x 2",
            "def c x 3",
            "def d x 4"
        );
    	assertStringResult("+ get x (a) (b) (c) (d)", "10");
	}

    @Test
	public void test_33() throws Throwable {
        __(
            "def a (z) (x 1)",
            "def b (z) (x 2)",
            "def c (z) (x 3)",
            "def d (z) (x 4)"
        );
    	assertStringResult("+ get x all z", "10");
	}

    @Test
	public void test_34() throws Throwable {
    	assertStringResult("+ 1,2", "3");
    	assertStringResult("* 6,2", "12");
	}

    @Test
    @Ignore
	public void test_35() throws Throwable {
        __(
            "def a 2",
            "def b 3"
        );
    	assertStringResult("+ a,b", "5");
	}

    @Test
    @Ignore //ignore for now
	public void test_40() throws Throwable {
    	assertStringResult("+ (* (10) (a)) (* (10) (b))", "+ (* (10) (a)) (* (10) (b))");
	}

    @Test
    @Ignore //ignore for now
	public void test_41() throws Throwable {
    	assertStringResult("+ (* (10) (a)) (* (10) (a))", "* (20) (a)");
	}
}