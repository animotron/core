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
import org.animotron.expression.Expression;
import org.junit.Test;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class AnTest extends ATest {

    @Test
    public void testAN() throws Throwable {


        tAnimo("def A v 'B'.");

        Expression test = tAnimo("A.");
        assertAnimoResultOneStep(test, "A v \"B\".");
    }

    @Test
    public void test_00() throws Throwable {

        testAnimo("def e f.");
        testAnimo("def d e.");
        testAnimo("def c d.");
        testAnimo("def b c.");
        testAnimo("def a b.");

        assertAnimoResult("a", "a b c d e f.");
    }

    @Test
    public void test_01() throws Throwable {

        testAnimo("def e all a.");
        testAnimo("def d a.");
        testAnimo("def c a.");
        testAnimo("def b a.");

        assertAnimoResult("e", "e (d a) (c a) (b a).");
    }

}