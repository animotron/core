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
import org.animotron.statement.operator.compare.WITH;
import org.animotron.statement.operator.query.ANY;
import org.animotron.statement.operator.relation.HAVE;
import org.animotron.statement.operator.relation.IS;
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
        assertAnimo(a, "<the:a><the:A><is:S/><have:X>α</have:X></the:A></the:a>");

        Expression b = new Expression(
            _(THE._, "b", _(ANY._, "S", _(WITH._, "Y", text("β"))))
        );
        assertAnimo(b, "<the:b><the:B><is:A/><have:Y>β</have:Y></the:B></the:b>");

        Expression c = new Expression(
            _(THE._, "c", _(ANY._, "S", _(WITH._, "Z", text("γ"))))
        );
        assertAnimo(c, "<the:c><the:C><is:B/><have:Z>γ</have:Z></the:C></the:c>");
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
        assertAnimo(a, "<the:a><the:A><is:S/><have:X>α</have:X></the:A></the:a>");

        Expression b = new Expression(
            _(THE._, "b", _(ANY._, "S", _(WITH._, "X", text("β"))))
        );
        assertAnimo(b, "<the:b><the:B><is:A/><have:X>β</have:X></the:B></the:b>");

        Expression c = new Expression(
            _(THE._, "c", _(ANY._, "S", _(WITH._, "X", text("γ"))))
        );
        assertAnimo(c, "<the:c><the:C><is:B/><have:X>γ</have:X></the:C></the:c>");
    }
}