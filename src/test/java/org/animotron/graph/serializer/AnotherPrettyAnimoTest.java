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
import org.junit.Test;

import static org.animotron.Properties.HASH;


/**
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class AnotherPrettyAnimoTest extends ATest {

    private void test(String exp) throws Exception {
        test(exp, exp);
    }

    private void test(String in, String out) throws Exception {
        AnimoExpression expression = new AnimoExpression(in);
        assertEquals((byte[]) HASH.get(expression), DigestSerializer._.serialize(expression));
        assertAnimo(expression, out + "\n", true);
    }

    @Test
	public void test_00() throws Exception {
        test("the a get b c.", "the a\n    get b c.");
	}

    @Test
	public void test_01() throws Exception {
        test("the a get an b an c", "the a\n    get an b c.");
	}

    @Test
	public void test_02() throws Exception {
        test("the a get an b c", "the a\n    get an b c.");
	}

    @Test
	public void test_03() throws Exception {
        test("the a get (b) (c)", "the a\n    get b c.");
	}

    @Test
	public void test_04() throws Exception {
        test("the a\n    an\n        (all b)\n        (all c).");
	}

    @Test
	public void test_05() throws Exception {
        test("the a\n    b.");
	}

    @Test
	public void test_06() throws Exception {
        test("an a b c", "a\n    b c.");
	}

    @Test
	public void test_07() throws Exception {
        test("a b c.", "a\n    b c.");
	}

    @Test
    public void test_08() throws Exception {
    	test("get (get a) (an b)", "get\n    (get a)\n    (b).");
    }

    @Test
    public void test_09() throws Exception {
    	test("all a,b ((c,d) (e))", "all a, b\n    ((c, d)\n        (e)).");
    }

    @Test
    public void test_10() throws Exception {
		test("all a,b an c,d e", "all a, b\n    c, d e.");
    }

    @Test
    public void test_11() throws Exception {
		test("all a, b\n    c, d e.");
    }

    @Test
    public void test_12() throws Exception {
		test("all a, b\n    c, d\n        (e)\n        (f).");
    }

}
