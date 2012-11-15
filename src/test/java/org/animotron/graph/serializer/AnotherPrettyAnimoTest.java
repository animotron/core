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
import org.junit.Test;

/**
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class AnotherPrettyAnimoTest extends ATest {

    private void test(String exp) throws Throwable {
        test(exp, exp);
    }

    private void test(String in, String out) throws Throwable {
        AnimoExpression expression = new AnimoExpression(in);
        assertAnimo(expression, out + "\n", true);
    }

    @Test
	public void test_00() throws Throwable {
        test("def a get b c.", "def a\n    get b c.");
	}

    @Test
	public void test_01() throws Throwable {
        test("def a get an b an c", "def a\n    get an b c.");
	}

    @Test
	public void test_02() throws Throwable {
        test("def a get an b c", "def a\n    get an b c.");
	}

    @Test
	public void test_03() throws Throwable {
        test("def a get (b) (c)", "def a\n    get b c.");
	}

    @Test
	public void test_04() throws Throwable {
        test("def a\n    an\n        (all b)\n        (all c).");
	}

    @Test
	public void test_05() throws Throwable {
        test("def a\n    b.");
	}

    @Test
	public void test_06() throws Throwable {
        test("an a b c", "a\n    b c.");
	}

    @Test
	public void test_07() throws Throwable {
        test("a b c.", "a\n    b c.");
	}

    @Test
    public void test_08() throws Throwable {
    	test("get (get a) (an b)", "get\n    (get a)\n    (b).");
    }

    @Test
    public void test_09() throws Throwable {
    	test("all a,b ((c,d) (e))", "all a, b\n    ((c, d)\n        (e)).");
    }

    @Test
    public void test_10() throws Throwable {
		test("all a,b an c,d e", "all a, b\n    c, d e.");
    }

    @Test
    public void test_11() throws Throwable {
		test("all a, b\n    c, d e.");
    }

    @Test
    public void test_12() throws Throwable {
		test("all a, b\n    c, d\n        (e)\n        (f).");
    }

}
