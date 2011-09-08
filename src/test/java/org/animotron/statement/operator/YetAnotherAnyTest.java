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
import org.animotron.Expression;
import org.animotron.statement.compare.WITH;
import org.animotron.statement.query.ANY;
import org.animotron.statement.relation.HAVE;
import org.animotron.statement.relation.IS;
import org.junit.Test;

import static org.animotron.Expression._;
import static org.animotron.Expression.text;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class YetAnotherAnyTest extends ATest {

    @Test
    public void deep_any() throws Exception {

        new Expression(
            _(THE._, "A", _(IS._, "S"), _(HAVE._, "X", text("α")))
        );

        new Expression(
            _(THE._, "B", _(IS._, "A"), _(HAVE._, "Y", text("β")))
        );

        new Expression(
            _(THE._, "C", _(IS._, "B"), _(HAVE._, "Z", text("γ")))
        );

        Expression a = new Expression(
            _(THE._, "a", _(ANY._, "S", _(WITH._, "X", text("α"))))
        );
        assertAnimoResult(a, "the a the A (is S) (have X \"α\")");

        Expression b = new Expression(
            _(THE._, "b", _(ANY._, "S", _(WITH._, "Y", text("β"))))
        );
        assertAnimoResult(b, "the b the B (is A) (have Y \"β\")");

        Expression c = new Expression(
            _(THE._, "c", _(ANY._, "S", _(WITH._, "Z", text("γ"))))
        );
        assertAnimoResult(c, "the c the C (is B) (have Z \"γ\")");
    }

    @Test
    public void one_more_deep_any() throws Exception {

        new Expression(
            _(THE._, "A", _(IS._, "S"), _(HAVE._, "X", text("α")))
        );

        new Expression(
            _(THE._, "B", _(IS._, "A"), _(HAVE._, "X", text("β")))
        );

        new Expression(
            _(THE._, "C", _(IS._, "B"), _(HAVE._, "X", text("γ")))
        );

        Expression a = new Expression(
            _(THE._, "a", _(ANY._, "S", _(WITH._, "X", text("α"))))
        );
        assertAnimoResult(a, "the a the A (is S) (have X \"α\")");

        Expression b = new Expression(
            _(THE._, "b", _(ANY._, "S", _(WITH._, "X", text("β"))))
        );
        assertAnimoResult(b, "the b the B (is A) (have X \"β\")");

        Expression c = new Expression(
            _(THE._, "c", _(ANY._, "S", _(WITH._, "X", text("γ"))))
        );
        assertAnimoResult(c, "the c the C (is B) (have X \"γ\")");
    }
}