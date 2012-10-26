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
import org.animotron.expression.JExpression;
import org.animotron.statement.compare.EQ;
import org.animotron.statement.compare.WITH;
import org.animotron.statement.query.ANY;
import org.junit.Test;

import static org.animotron.expression.AnimoExpression.__;
import static org.animotron.expression.JExpression._;
import static org.animotron.expression.JExpression.value;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class PathTest extends ATest {

    @Test
    public void test_00() throws Throwable {
        testAnimo("def a b (c d) (e).");
        testAnimo("def x b c d e.");
        testAnimoResult("// a, x b c.", "c. c.");
    }

    @Test
    public void test_01() throws Throwable {
        testAnimo("def b 1.");
        testAnimo("def a b 1.");
        testAnimoResult("// a b 1.", "1.");
    }

    @Test
    public void test_02() throws Throwable {
        testAnimo("def a b c, d.");
        testAnimoResult("// a b c, d.", "c. d.");
    }

    @Test
    public void test_03() throws Throwable {
        testAnimo("def a b c.");
        testAnimoResult("// a c.", "");
    }

}

