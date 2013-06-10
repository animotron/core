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
package org.animotron.statement.operator;

import org.animotron.ATest;
import org.junit.Ignore;
import org.junit.Test;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class DiamondTest extends ATest {

    @Test
    @Ignore
    public void test_001a() throws Throwable {
        testAnimo("def foo x.");
        testAnimo("def bar x.");
        testAnimo("def a use foo.");
        testAnimo("def b use bar.");
        testAnimo("def c (a, b) (any x).");
        assertAnimoResult("any a", "c (a use foo) (b use bar) (foo x).");
        assertAnimoResult("any b", "c (a use foo) (b use bar) (bar x).");
    }

    @Test
    @Ignore
    public void test_001b() throws Throwable {
        testAnimo("def foo x.");
        testAnimo("def bar x.");
        testAnimo("def a use foo.");
        testAnimo("def b use bar.");
        testAnimo("def c (a) (b) (any x).");
        assertAnimoResult("any a", "c (a use foo) (b use bar) (foo x).");
        assertAnimoResult("any b", "c (a use foo) (b use bar) (bar x).");
    }

    @Test
    @Ignore
    public void test_002a() throws Throwable {
        testAnimo("def foo x.");
        testAnimo("def bar x.");
        testAnimo("def a use foo.");
        testAnimo("def b use bar.");
        testAnimo("def c any x.");
        testAnimo("def d a, b, c.");
        assertAnimoResult("any a", "d (a use foo) (b use bar) (c foo x).");
        assertAnimoResult("any b", "d (a use foo) (b use bar) (c bar x).");
    }

    @Test
    @Ignore
    public void test_002b() throws Throwable {
        testAnimo("def foo x.");
        testAnimo("def bar x.");
        testAnimo("def a use foo.");
        testAnimo("def b use bar.");
        testAnimo("def c any x.");
        testAnimo("def d (a, b) (c).");
        assertAnimoResult("any a", "d (a use foo) (b use bar) (c foo x).");
        assertAnimoResult("any b", "d (a use foo) (b use bar) (c bar x).");
    }

    @Test
    @Ignore
    public void test_002c() throws Throwable {
        testAnimo("def foo x.");
        testAnimo("def bar x.");
        testAnimo("def a use foo.");
        testAnimo("def b use bar.");
        testAnimo("def c any x.");
        testAnimo("def d (a) (b) (c).");
        assertAnimoResult("any a", "d (a use foo) (b use bar) (c foo x).");
        assertAnimoResult("any b", "d (a use foo) (b use bar) (c bar x).");
    }

    @Test
    @Ignore
    public void test_005a() throws Throwable {
        testAnimo("def a x foo.");
        testAnimo("def b x bar.");
        testAnimo("def c (a, b) (get x).");
        assertAnimoResult("any a", "c (a x) (b x) (x foo).");
        assertAnimoResult("any b", "c (a x) (b x) (x bar).");
    }

    @Test
    @Ignore
    public void test_005b() throws Throwable {
        testAnimo("def a x foo.");
        testAnimo("def b x bar.");
        testAnimo("def c (a) (b) (get x).");
        assertAnimoResult("any a", "c (a x) (b x) (x foo).");
        assertAnimoResult("any b", "c (a x) (b x) (x bar).");
    }

    @Test
    @Ignore
    public void test_006a() throws Throwable {
        testAnimo("def a x foo.");
        testAnimo("def b x bar.");
        testAnimo("def c get x.");
        testAnimo("def d a, b, c.");
        assertAnimoResult("any a", "d (a x) (b x) (c x foo).");
        assertAnimoResult("any b", "d (a x) (b x) (c x bar).");
    }

    @Test
    @Ignore
    public void test_006b() throws Throwable {
        testAnimo("def a x foo.");
        testAnimo("def b x bar.");
        testAnimo("def c get x.");
        testAnimo("def d (a, b) (c).");
        assertAnimoResult("any a", "d (a x) (b x) (c x foo).");
        assertAnimoResult("any b", "d (a x) (b x) (c x bar).");
    }

    @Test
    @Ignore
    public void test_006c() throws Throwable {
        testAnimo("def a x foo.");
        testAnimo("def b x bar.");
        testAnimo("def c get x.");
        testAnimo("def d (a) (b) (c).");
        assertAnimoResult("any a", "d (a x) (b x) (c x foo).");
        assertAnimoResult("any b", "d (a x) (b x) (c x bar).");
    }
}