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
package org.animotron.statement.animo;

import org.animotron.ATest;
import org.junit.Test;

import java.io.IOException;

public class WhatISTest extends ATest {

	@Test
	public void test_00() throws IOException {
        __("def a x.");
        
        assertAnimoResult("what-is a", "x.");
	}

    @Test
    public void test_01() throws IOException {
        __("def a (x) (y).");

        assertAnimoResult("what-is a", "x. y.");
    }

//    @Test
//    public void test_02() throws IOException {
//        __("def a (x) (y) (z).");
//
//        assertAnimoResult("what-is a use y", "y.");
//    }

    @Test
    public void test_03() throws IOException {
        __("def a x.", "def b y.", "def c b a.");

        assertAnimoResult("what-is get b c", "x.");

        assertAnimoResult("each (get b c) (what-is this b)", "x.");
    }

    @Test
    public void test_04() throws IOException {
        __("def a x.", "def b y.", "def c part (a) (b).");

        assertAnimoResult("each (get part c) (what-is this part)", "x. y.");
    }

    @Test
    public void test_05() throws IOException {
        __("def a x.", "def b y.", "def c part (a) (b).", "def part foo.");

        assertAnimoResult("each (get part c) (what-is this part)", "x. y.");//foo.???
    }

    @Test
    public void test_06() throws IOException {
        __("def foo bar.", "def a bar x.");

        assertAnimoResult("get (what-is foo) (a)", "x.");
    }

}