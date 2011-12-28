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
 *  but WITHOUT ALL WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */
package org.animotron.statement.operator;

import org.animotron.ATest;
import org.junit.Ignore;
import org.junit.Test;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class IdTest extends ATest {

    @Test
    public void test_00() throws Exception {
        assertAnimoResult("id a", "\"a\".");
    }

    @Test
    public void test_01() throws Exception {
        assertAnimoResult("id a, b", "\"a\". \"b\".");
    }

    @Test
    public void test_02() throws Exception {
        testAnimo("the a (x) (y 1).");
        testAnimo("the b (x) (y 2).");
        testAnimo("the c (x) (y 3).");
        assertAnimoResult("id all x", "\"a\". \"b\". \"c\".");
    }

    @Test
    @Ignore
    public void test_03() throws Exception {
        testAnimo("the a (x) (y 1).");
        assertAnimoResult("id get y a", "\"y\".");
    }

    @Test
    public void test_04() throws Exception {
        testAnimo("the a (x) (y 1).");
        testAnimo("the b (x) (y 2).");
        testAnimo("the c (x) (y 3).");
        assertAnimoResult("id any x with y 2", "\"b\".");
    }

    @Test
    public void test_05() throws Exception {
        testAnimo("the a (x) (y 1).");
        testAnimo("the b (x) (y 2).");
        testAnimo("the c (x) (y 3).");
        assertAnimoResult("each (all x) (\\foo id this x)", "\\foo \"a\". \\foo \"b\". \\foo \"c\". ");
        //Or so?
        assertAnimoResult("each (id all x) (\\foo)", "\\foo \"a\". \\foo \"b\". \\foo \"c\". ");
    }

}
