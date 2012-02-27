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
package org.animotron.statement.operator;

import org.animotron.ATest;
import org.junit.Test;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class WaveGetTest extends ATest {

    @Test
    public void test_000() throws Throwable {
        testAnimo("the a (x) (foo 1).");
        testAnimo("the y (a) (get foo).");
        assertAnimoResult("y", "y (a (x) (foo)) (foo 1).");
    }

    @Test
    public void test_001() throws Throwable {
        testAnimo("the a (x) (foo 1).");
        testAnimo("the y (any x) (get foo).");
        assertAnimoResult("y", "y (the a (x) (foo)) (foo 1).");
    }

    @Test
    public void test_002() throws Throwable {
        testAnimo("the a (x) (foo 1).");
        testAnimo("the y (all x) (get foo).");
        assertAnimoResult("y", "y (the a (x) (foo)) (foo 1).");
    }

    @Test
    public void test_003() throws Throwable {
        testAnimo("the a (x) (foo 1).");
        testAnimo("the y (prefer x use a) (get foo).");
        assertAnimoResult("y", "y (the a (x) (foo)) (foo 1).");
    }

    @Test
    public void test_004() throws Throwable {
        testAnimo("the a (x) (foo 1).");
        testAnimo("the y get foo.");
        testAnimo("the z y any x.");
        assertAnimoResult("z", "z y foo 1.");
    }
}