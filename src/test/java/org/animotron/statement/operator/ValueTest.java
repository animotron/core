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

import junit.framework.Assert;
import org.animotron.ATest;
import org.junit.Ignore;
import org.junit.Test;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class ValueTest extends ATest {

    @Test
    @Ignore
    public void test() throws Throwable {

        testAnimo("def foo \"abc\".");
        testAnimo("def foo \"abe\".");
        testAnimo("def bar \"ebc\".");

        try {Thread.sleep(1000);} catch (InterruptedException e) {}

        Assert.assertNotNull(VALUE._.get("a"));
        Assert.assertNotNull(VALUE._.get("b"));
        Assert.assertNotNull(VALUE._.get("c"));

    }

    @Test
    public void test_00() throws Throwable {

        testAnimo("def foo \"abc\".");
        assertStringResult("foo", "abc");

    }

    @Test
    public void test_01() throws Throwable {

        testAnimo("def foo (x) \"abc\".");
        assertStringResult("any x", "abc");

    }

    @Test
    public void test_02() throws Throwable {

        testAnimo("def foo (x) \"abc\".");
        testAnimo("def bar (x) \"def\".");
        assertStringResult("all x", "abcdef");

    }

}