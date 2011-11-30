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
public class PrettyAnimoTest extends ATest {

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
        test("the a.");
	}

    @Test
	public void test_01() throws Exception {
        test("the a b.");
	}

    @Test
	public void test_02() throws Exception {
        test("the a\n    b \"test\".");
	}

    @Test
	public void test_03() throws Exception {
        test("the a (b) (c)", "the a\n    (b)\n    (c).");
	}

    @Test
	public void test_04() throws Exception {
        test("the a\n    (b\n        (any x)\n        (all y))\n    (c).");
	}

    @Test
	public void test_05() throws Exception {
        test("the a\n    b\n        (any x)\n        (all y).");
	}


    @Test
	public void test_06() throws Exception {
        test("the a\n    b\n        (any x α)\n        (all y β).");
	}

    @Test
    public void test_07() throws Exception {
        test("the a @b (get c)", "the a\n    @b\n        get c.");
    }

    @Test
    public void test_08() throws Exception {
        test("the a\n    \\b \\c @d e.");
    }

    @Test
    public void test_09() throws Exception {
        test("the a\n    \\b \\c\n        @d\n            (e)\n            (f).");
    }

    @Test
    public void test_0A() throws Exception {
        test(
            "the rest\n" +
            "    content\n" +
            "        \\html\n" +
            "            (\\head \\title title \"Welcome to Animo\")\n" +
            "            (\\body\n" +
            "                theme-concrete-root-layout\n" +
            "                    (root-layout)\n" +
            "                    (\\h1 title \"Welcome to Animo\")\n" +
            "                    (\\p content \"It is working\")\n" +
            "                    (\\ul\n" +
            "                        (\\li\n" +
            "                            \"host:\"\n" +
            "                            (\\strong host \"localhost\"))\n" +
            "                        (\\li\n" +
            "                            \"uri:\"\n" +
            "                            (\\strong uri \"/\"))))."
        );
    }

}
