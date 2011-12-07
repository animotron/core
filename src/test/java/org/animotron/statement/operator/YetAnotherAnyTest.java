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
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
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
import org.animotron.statement.compare.WITH;
import org.animotron.statement.query.ANY;
import org.junit.Test;

import static org.animotron.expression.JExpression._;
import static org.animotron.expression.JExpression.text;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class YetAnotherAnyTest extends ATest {

    @Test
    public void deep_any() throws Exception {

        JExpression.__(
                new JExpression(
                        _(THE._, "A", _(AN._, "S"), _(AN._, "X", text("α")))
                ),
                new JExpression(
                        _(THE._, "B", _(AN._, "A"), _(AN._, "Y", text("β")))
                ),
                new JExpression(
                        _(THE._, "C", _(AN._, "B"), _(AN._, "Z", text("γ")))
                )
        );

        JExpression a = new JExpression(
            _(THE._, "a", _(ANY._, "S", _(WITH._, "X", text("α"))))
        );
        assertAnimoResultOneStep(a, "the a the A (S) (X \"α\").");

        JExpression b = new JExpression(
            _(THE._, "b", _(ANY._, "S", _(WITH._, "Y", text("β"))))
        );
        assertAnimoResultOneStep(b, "the b the B (A) (Y \"β\").");

        JExpression c = new JExpression(
            _(THE._, "c", _(ANY._, "S", _(WITH._, "Z", text("γ"))))
        );
        assertAnimoResultOneStep(c, "the c the C (B) (Z \"γ\").");
    }

    @Test
    public void one_more_deep_any() throws Exception {

        JExpression.__(
                new JExpression(
                        _(THE._, "A", _(AN._, "S"), _(AN._, "X", text("α")))
                ),
                new JExpression(
                        _(THE._, "B", _(AN._, "A"), _(AN._, "X", text("β")))
                ),
                new JExpression(
                        _(THE._, "C", _(AN._, "B"), _(AN._, "X", text("γ")))
                )
        );

        JExpression a = new JExpression(
            _(THE._, "a", _(ANY._, "S", _(WITH._, "X", text("α"))))
        );
        assertAnimoResultOneStep(a, "the a the A (S) (X \"α\").");

        JExpression b = new JExpression(
            _(THE._, "b", _(ANY._, "S", _(WITH._, "X", text("β"))))
        );
        assertAnimoResultOneStep(b, "the b the B (A) (X \"β\").");

        JExpression c = new JExpression(
            _(THE._, "c", _(ANY._, "S", _(WITH._, "X", text("γ"))))
        );
        assertAnimoResultOneStep(c, "the c the C (B) (X \"γ\").");
    }
}