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
import org.animotron.expression.AnimoExpression;
import org.animotron.expression.Expression;
import org.animotron.expression.JExpression;
import org.animotron.statement.query.GET;
import org.animotron.statement.relation.SHALL;
import org.junit.Ignore;
import org.junit.Test;

import static org.animotron.expression.Expression.__;
import static org.animotron.expression.JExpression.*;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class GetNTest extends ATest {

    @Test
    public void test_000() throws Throwable {
        testAnimo("def a get 1.");
        assertAnimoResult("a b", "a b.");
    }

    @Test
    public void test_001() throws Throwable {
        testAnimo("def \\x a get 1.");
        assertAnimoResult("a b", "a \\x b.");
    }

    @Test
    public void test_002() throws Throwable {
        testAnimo("def a get 1.");
        assertAnimoResult("a (b) (c)", "a (b) (c).");
    }

    @Test
    public void test_003() throws Throwable {
        testAnimo("def a \\x get 1.");
        assertAnimoResult("a (b) (c)", "a \\x (b) (c).");
    }

}
