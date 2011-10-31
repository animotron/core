/*
 *  Copyright (C) 2011 The Animo Project
 *  http://animotron.org
 *
 *  This program is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public License
 *  as published by the Free Software Foundation; either version 3
 *  of the License, or (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ALL WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */
package org.animotron.statement.operator;

import org.animotron.ATest;
import org.animotron.expression.JExpression;
import org.junit.Test;

import static org.animotron.expression.JExpression.*;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class EachTest extends ATest {

    @Test
    public void test_00() throws Exception {

        JExpression s = new JExpression(
            element("ul", each(_(_(AN._, "A"), _(AN._, "B"), _(AN._, "C")), element("li")))
        );
        assertAnimoResult(s, "\\ul (\\li the A) (\\li the B) (\\li the C)");

    }

    @Test
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
                        "(\\c3 \\b3 \\a3)"

       );

    }

}