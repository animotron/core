/*
 *  Copyright (C) 2011-2013 The Animo Project
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
public class PrettyAnimoTest extends ATest {

    private void test(String exp) throws Throwable {
        test(exp, exp);
    }

    private void test(String in, String out) throws Throwable {
        AnimoExpression expression = new AnimoExpression(in);
        assertAnimo(expression, out + "\n", true);
    }

    @Test
	public void test_00() throws Throwable {
        test("def a.");
	}

    @Test
	public void test_01() throws Throwable {
        test("def a\n    b.");
	}

    @Test
	public void test_02() throws Throwable {
        test("def a\n    b \"test\".");
	}

    @Test
	public void test_03() throws Throwable {
        test("def a (b) (c)", "def a\n    (b)\n    (c).");
	}

    @Test
	public void test_04() throws Throwable {
        test("def a\n    (b\n        (any x)\n        (all y))\n    (c).");
	}

    @Test
	public void test_05() throws Throwable {
        test("def a\n    b\n        (any x)\n        (all y).");
	}


    @Test
	public void test_06() throws Throwable {
        test("def a\n    b\n        (any x α)\n        (all y β).");
	}

    @Test
    public void test_07() throws Throwable {
        test("def a @b (get c)", "def a\n    @b\n        get c.");
    }

    @Test
    public void test_0B() throws Throwable {
        test("an\n" +
                "    get x.");
    }

    @Test
    public void test_0C() throws Throwable {
        test("an\n" +
                "    (get x)\n" +
                "    (y).");
    }

    @Test
    public void test_0D() throws Throwable {
        testAnimo("def z an (get x) (y).");
    }

    @Test
    public void test_0E() throws Throwable {
        testAnimo("def z (an (get x) (y)) (an (any b) (c)).");
    }

}
