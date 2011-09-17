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
import org.animotron.exception.AnimoException;
import org.animotron.graph.builder.AnimoBuilder;
import org.junit.Test;

import java.io.IOException;


/**
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class PrettyAnimoTest extends ATest {

    private void test(String exp) throws AnimoException, IOException {
        test(exp, exp);
    }

    private void test(String in, String out) throws AnimoException, IOException {
        AnimoBuilder builder = new AnimoBuilder(in);
        builder.build();
        assertAnimo(builder.getRelationship(), out + "\n", true);
    }

    @Test
	public void test_00() throws IOException, AnimoException {
        test("the a");
	}

    @Test
	public void test_01() throws IOException, AnimoException {
        test("the a have b");
	}

    @Test
	public void test_02() throws IOException, AnimoException {
        test("the a have b \"test\"");
	}

    @Test
	public void test_03() throws IOException, AnimoException {
        test("the a (have b) (have c)", "the a\n    (have b)\n    (have c)");
	}

    @Test
	public void test_04() throws IOException, AnimoException {
        test("the a\n    (have b\n        (any x)\n        (all y))\n    (have c)");
	}

    @Test
	public void test_05() throws IOException, AnimoException {
        test("the a\n    have b\n        (any x)\n        (all y)");
	}


    @Test
	public void test_06() throws IOException, AnimoException {
        test("the a\n    have b\n        (any x α)\n        (all y β)");
	}

    @Test
    public void test_07() throws IOException, AnimoException {
        test("the a @b (get c)", "the a @b get c");
    }

    @Test
    public void test_08() throws IOException, AnimoException {
        test("the a \\b \\c @d e");
    }

    @Test
    public void test_09() throws IOException, AnimoException {
        test("the a\n" +
             "    \\b\n" +
             "        \\c\n" +
             "            @d\n" +
             "                (e)\n" +
             "                (f)");
    }

}
