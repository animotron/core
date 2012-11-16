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
import org.junit.Test;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class AnyUseTest extends ATest {

    @Test
    public void simple_any_Use() throws Throwable {

        tAnimo("def A (^S) (\\X 'α').");
        tAnimo("def B (^A) (\\Y 'β').");
        tAnimo("def C (^B) (\\Z 'γ') (\\X 'αα').");
        tAnimo("def s (any S).");

        Expression b = new AnimoExpression("def b s use B");
        //assertAnimoResult(b, "b s the B (A (S) (\\X \"α\")) (\\Y \"β\").");
//        assertAnimoResult(b, "b s the C (B (A (S) (\\X \"α\")) (\\Y \"β\")) (\\Z \"γ\") (\\X \"αα\").");
        assertAnimoResult(b, "b s B (A (S) (\\X \"α\")) (\\Y \"β\").");

        Expression c = new AnimoExpression("def c s use C");
        assertAnimoResult(c, "c s C (B (A (S) (\\X \"α\")) (\\Y \"β\")) (\\Z \"γ\") (\\X \"αα\").");
    }

    @Test
    public void complex_any_Use() throws Throwable {

        tAnimo("def A (^S) (\\X 'α').");
        tAnimo("def B (^A) (\\Y 'β').");
        tAnimo("def B1 (^B) (\\Y 'ββ').");
        tAnimo("def C (^B) (\\Z 'γ') (\\X 'αα').");
        tAnimo("def C1 (^C) (\\Z 'γγ') (\\X 'ααα').");
        tAnimo("def s (any S).");

        Expression b = new AnimoExpression("def b s use B");
        //assertAnimoResult(b, "b s the B (A (S) (\\X \"α\")) (\\Y \"β\").");
//        assertAnimoResult(b, "b s B1 (B (A (S) (\\X \"α\")) (\\Y \"β\")) (\\Y \"ββ\").");
        assertAnimoResult(b, "b s B (A (S) (\\X \"α\")) (\\Y \"β\").");

        Expression c = new AnimoExpression("def b s use C");
        //assertAnimoResult(c, "c s the C (B (A (S) (\\X \"α\")) (\\Y \"β\")) (\\Z \"γ\") (\\X \"αα\").");
//        assertAnimoResult(c, "c s C1 (C (B (A (S) (\\X \"α\")) (\\Y \"β\")) (\\Z \"γ\") (\\X \"αα\")) (\\Z \"γγ\") (\\X \"ααα\").");
        assertAnimoResult(c, "c s C (B (A (S) (\\X \"α\")) (\\Y \"β\")) (\\Z \"γ\") (\\X \"αα\").");
    }
}