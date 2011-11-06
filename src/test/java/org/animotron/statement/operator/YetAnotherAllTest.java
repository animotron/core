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
import org.animotron.statement.compare.WITH;
import org.animotron.statement.query.ALL;
import org.animotron.statement.relation.HAVE;
import org.animotron.statement.relation.IS;
import org.junit.Test;

import static org.animotron.expression.JExpression._;
import static org.animotron.expression.JExpression.text;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class YetAnotherAllTest extends ATest {

    @Test
    public void deep_all() throws Exception {

    	JExpression.__(
                new JExpression(
                        _(THE._, "A", _(IS._, "S"), _(HAVE._, "X", text("α")))
                ),
                new JExpression(
                        _(THE._, "B", _(IS._, "A"), _(HAVE._, "Y", text("β")))
                ),
                new JExpression(
                        _(THE._, "C", _(IS._, "B"), _(HAVE._, "Z", text("γ")), _(HAVE._, "X", text("αα")))
                )
        );

        JExpression a = new JExpression(
            _(THE._, "a", _(ALL._, "S", _(WITH._, "X", text("α"))))
        );
        assertAnimoResult(a, "the a (the A (is S) (have X \"α\")) (the B (is A) (have Y \"β\")).");

        JExpression b = new JExpression(
            _(THE._, "b", _(ALL._, "S", _(WITH._, "Y", text("β"))))
        );
        assertAnimoResult(b, "the b (the B (is A) (have Y \"β\")) (the C (is B) (have Z \"γ\") (have X \"αα\")).");

        JExpression c = new JExpression(
            _(THE._, "c", _(ALL._, "S", _(WITH._, "Z", text("γ"))))
        );
        assertAnimoResult(c, "the c the C (is B) (have Z \"γ\") (have X \"αα\").");

    }

    @Test
    public void one_more_deep_all() throws Exception {

        JExpression.__(
                new JExpression(
                        _(THE._, "A", _(IS._, "S"), _(HAVE._, "X", text("α")))
                ),
                new JExpression(
                        _(THE._, "B", _(IS._, "A"), _(HAVE._, "X", text("β")))
                ),
                new JExpression(
                        _(THE._, "C", _(IS._, "B"), _(HAVE._, "X", text("γ")))
                )
        );

        JExpression a = new JExpression(
            _(THE._, "a", _(ALL._, "S", _(WITH._, "X", text("α"))))
        );
        assertAnimoResult(a, "the a the A (is S) (have X \"α\").");

        JExpression b = new JExpression(
            _(THE._, "b", _(ALL._, "S", _(WITH._, "X", text("β"))))
        );
        assertAnimoResult(b, "the b the B (is A) (have X \"β\").");

        JExpression c = new JExpression(
            _(THE._, "c", _(ALL._, "S", _(WITH._, "X", text("γ"))))
        );
        assertAnimoResult(c, "the c the C (is B) (have X \"γ\").");

    }

}