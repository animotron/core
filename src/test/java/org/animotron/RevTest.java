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
package org.animotron;

import org.junit.Test;

/**
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class RevTest extends ATest {

    @Test
    public void test_00() throws Throwable {
        testAnimo("def a x.");
        testAnimo("def a y.");
        testAnimo("def a x.");
    }

    @Test
    public void test_01() throws Throwable {
        testAnimo("def a x.");
        testAnimo("def a y.");
        testAnimo("def a z.");
        testAnimo("def a y.");
        testAnimo("def a x.");
    }

    @Test
    public void test_02() throws Throwable {
        testAnimo("def a x.");
        testAnimo("def b x.");
        testAnimo("def a y.");
        testAnimo("def b y.");
//        testAnimo("def a z.");
//        testAnimo("def b z.");
    }

    @Test
    public void test_03() throws Throwable {
        testAnimo("def a x.");
        testAnimo("def a y.");
        testAnimo("def a z.");
        testAnimo("def a y.");
        testAnimo("def a x.");
        testAnimo("def b x.");
        testAnimo("def b y.");
        testAnimo("def b z.");
        testAnimo("def b y.");
        testAnimo("def b x.");
    }

    @Test
    public void test_04() throws Throwable {
        testAnimo("def a.");
        testAnimo("def a x.");
        assertAnimoResult("a", "a x.");
        assertAnimoResult("all x", "a x.");
    }

    @Test
    public void test_05() throws Throwable {
        testAnimo("def a.");
        testAnimo("def a x y.");
        assertAnimoResult("get x a", "y.");
    }

    @Test
    public void test_06() throws Throwable {
        testAnimo("def a.");
        testAnimo("def a b.");
        testAnimo("def c a.");
        assertAnimoResult("get b c", "a.");
    }

    @Test
    public void test_07() throws Throwable {
        testAnimo("def c.");
        testAnimo("def a ^b.");
        testAnimo("def c ^a.");
        assertAnimoResult("get b c", "a.");
    }

    @Test
    public void test_08() throws Throwable {
        testAnimo("def c.");
        testAnimo("def a ^b.");
        testAnimo("def c a.");
        assertAnimoResult("get b c", "a.");
    }

    @Test
    public void test_09() throws Throwable {
        testAnimo("def c.");
        testAnimo("def a b.");
        testAnimo("def c ^a.");
        assertAnimoResult("get b c", "a.");
    }
}
