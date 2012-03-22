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
public class DiamondTest extends ATest {

    @Test
    public void test_001a() throws Throwable {
        testAnimo("the foo x.");
        testAnimo("the bar x.");
        testAnimo("the a use foo.");
        testAnimo("the b use bar.");
        testAnimo("the c (a, b) (any x).");
        assertAnimoResult("any a", "the c (a use foo) (b use bar) (the foo x).");
        assertAnimoResult("any b", "the c (a use foo) (b use bar) (the bar x).");
    }

    @Test
    public void test_001b() throws Throwable {
        testAnimo("the foo x.");
        testAnimo("the bar x.");
        testAnimo("the a use foo.");
        testAnimo("the b use bar.");
        testAnimo("the c (a) (b) (any x).");
        assertAnimoResult("any a", "the c (a use foo) (b use bar) (the foo x).");
        assertAnimoResult("any b", "the c (a use foo) (b use bar) (the bar x).");
    }

    @Test
    public void test_002a() throws Throwable {
        testAnimo("the foo x.");
        testAnimo("the bar x.");
        testAnimo("the a use foo.");
        testAnimo("the b use bar.");
        testAnimo("the c any x.");
        testAnimo("the d a, b, c.");
        assertAnimoResult("any a", "the c (a use foo) (b use bar) (c the foo x).");
        assertAnimoResult("any b", "the c (a use foo) (b use bar) (c the bar x).");
    }

    @Test
    public void test_002b() throws Throwable {
        testAnimo("the foo x.");
        testAnimo("the bar x.");
        testAnimo("the a use foo.");
        testAnimo("the b use bar.");
        testAnimo("the c any x.");
        testAnimo("the d (a, b) (c).");
        assertAnimoResult("any a", "the c (a use foo) (b use bar) (c the foo x).");
        assertAnimoResult("any b", "the c (a use foo) (b use bar) (c the bar x).");
    }

    @Test
    public void test_002c() throws Throwable {
        testAnimo("the foo x.");
        testAnimo("the bar x.");
        testAnimo("the a use foo.");
        testAnimo("the b use bar.");
        testAnimo("the c any x.");
        testAnimo("the d (a) (b) (c).");
        assertAnimoResult("any a", "the c (a use foo) (b use bar) (c the foo x).");
        assertAnimoResult("any b", "the c (a use foo) (b use bar) (c the bar x).");
    }

    @Test
    public void test_005a() throws Throwable {
        testAnimo("the a x foo.");
        testAnimo("the b x bar.");
        testAnimo("the c (a, b) (get x).");
        assertAnimoResult("any a", "the c (a x) (b x) (x foo).");
        assertAnimoResult("any b", "the c (a x) (b x) (x bar).");
    }

    @Test
    public void test_005b() throws Throwable {
        testAnimo("the a x foo.");
        testAnimo("the b x bar.");
        testAnimo("the c (a) (b) (get x).");
        assertAnimoResult("any a", "the c (a x) (b x) (x foo).");
        assertAnimoResult("any b", "the c (a x) (b x) (x bar).");
    }

    @Test
    public void test_006a() throws Throwable {
        testAnimo("the a x foo.");
        testAnimo("the b x bar.");
        testAnimo("the c get x.");
        testAnimo("the d a, b, c.");
        assertAnimoResult("any a", "the d (a x) (b x) (c x foo).");
        assertAnimoResult("any b", "the d (a x) (b x) (c x bar).");
    }

    @Test
    public void test_006b() throws Throwable {
        testAnimo("the a x foo.");
        testAnimo("the b x bar.");
        testAnimo("the c get x.");
        testAnimo("the d (a, b) (c).");
        assertAnimoResult("any a", "the d (a x) (b x) (c x foo).");
        assertAnimoResult("any b", "the d (a x) (b x) (c x bar).");
    }

    @Test
    public void test_006c() throws Throwable {
        testAnimo("the a x foo.");
        testAnimo("the b x bar.");
        testAnimo("the c get x.");
        testAnimo("the d (a) (b) (c).");
        assertAnimoResult("any a", "the d (a x) (b x) (c x foo).");
        assertAnimoResult("any b", "the d (a x) (b x) (c x bar).");
    }

}