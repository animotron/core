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
import org.junit.Ignore;
import org.junit.Test;

import static org.animotron.expression.JExpression.*;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class EachTest extends ATest {

    @Test
    public void test() throws Exception {
        testAnimo("the a (s) (x 1).");
        testAnimo("the b (s) (x 2).");
        testAnimo("the c (s) (x 3).");

        assertAnimoResult("each (all s) (\\foo get x)", "\\foo x 1. \\foo x 2. \\foo x 3.");
    }

    @Test
    @Ignore
    public void test_00() throws Exception {

        JExpression s = new JExpression(
                element("ul", each(_(_(AN._, "A"), _(AN._, "B"), _(AN._, "C")), element("li")))
        );
        assertAnimoResult(s, "\\ul (\\li the A) (\\li the B) (\\li the C).");

    }

    @Test
    @Ignore
    public void test_01() throws Exception {

        JExpression s = new JExpression(
            element("x",
                each(
                        _(element("a1"), element("a2"), element("a3")),
                        _(element("b1"), element("b2"), element("b3")),
                        _(element("c1"), element("c2"), element("c3"))
                )
            )
        );

        assertAnimoResult(s,

                "\\x " +

                        "(\\c1 \\b1 \\a1) " +
                        "(\\c1 \\b1 \\a2) " +
                        "(\\c1 \\b1 \\a3) " +

                        "(\\c1 \\b2 \\a1) " +
                        "(\\c1 \\b2 \\a2) " +
                        "(\\c1 \\b2 \\a3) " +

                        "(\\c1 \\b3 \\a1) " +
                        "(\\c1 \\b3 \\a2) " +
                        "(\\c1 \\b3 \\a3) " +

                        "(\\c2 \\b1 \\a1) " +
                        "(\\c2 \\b1 \\a2) " +
                        "(\\c2 \\b1 \\a3) " +

                        "(\\c2 \\b2 \\a1) " +
                        "(\\c2 \\b2 \\a2) " +
                        "(\\c2 \\b2 \\a3) " +

                        "(\\c2 \\b3 \\a1) " +
                        "(\\c2 \\b3 \\a2) " +
                        "(\\c2 \\b3 \\a3) " +

                        "(\\c3 \\b1 \\a1) " +
                        "(\\c3 \\b1 \\a2) " +
                        "(\\c3 \\b1 \\a3) " +

                        "(\\c3 \\b2 \\a1) " +
                        "(\\c3 \\b2 \\a2) " +
                        "(\\c3 \\b2 \\a3) " +

                        "(\\c3 \\b3 \\a1) " +
                        "(\\c3 \\b3 \\a2) " +
                        "(\\c3 \\b3 \\a3)."
       );
    }
}