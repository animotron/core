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
package org.animotron.operator;

import org.animotron.ATest;
import org.animotron.Expression;
import org.animotron.exception.EBuilderTerminated;
import org.animotron.operator.query.ANY;
import org.animotron.operator.relation.HAVE;
import org.animotron.operator.relation.IS;
import org.animotron.operator.relation.USE;
import org.junit.Test;

import java.io.IOException;

import static org.animotron.Expression._;
import static org.animotron.Expression.text;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class AnyUseTest extends ATest {

    @Test
    public void simple_any_Use() throws EBuilderTerminated, IOException, InterruptedException {
        new Expression(
            _(THE._, "A", _(IS._, "S"), _(HAVE._, "X", text("α")))
        );

        new Expression(
            _(THE._, "B", _(IS._, "A"), _(HAVE._, "Y", text("β")))
        );

        new Expression(
            _(THE._, "C", _(IS._, "B"), _(HAVE._, "Z", text("γ")),  _(HAVE._, "X", text("αα")))
        );

        new Expression (
            _(THE._, "s", _(ANY._, "S"))
        );

        Expression b = new Expression(
            _(THE._, "b", _(AN._, "s", _(USE._, "B")))
        );
        assertAnimo(b, "<the:b><the:s><the:B><is:A/><have:Y>β</have:Y></the:B></the:s></the:b>");

        Expression c = new Expression(
            _(THE._, "c", _(AN._, "s", _(USE._, "C")))
        );
        assertAnimo(c, "<the:c><the:s><the:C><is:B/><have:Z>γ</have:Z><have:X>αα</have:X></the:C></the:s></the:c>");

    }

    @Test
    public void simple_any_Use_1() throws EBuilderTerminated, IOException, InterruptedException {
        new Expression(
            _(THE._, "A", _(IS._, "S"), _(HAVE._, "X", text("α")))
        );

        new Expression(
            _(THE._, "B", _(IS._, "A"), _(HAVE._, "Y", text("β")))
        );

        new Expression(
            _(THE._, "C", _(IS._, "B"), _(HAVE._, "Z", text("γ")),  _(HAVE._, "X", text("αα")))
        );

        new Expression (
            _(THE._, "s", _(ANY._, "S"))
        );

        new Expression (
            _(THE._, "ub", _(USE._, "B"))
        );

        new Expression (
            _(THE._, "uc", _(USE._, "C"))
        );

        Expression b = new Expression(
            _(THE._, "b", _(AN._, "s", _(AN._, "ub")))
        );
        assertAnimo(b, "<the:b><the:s><the:B><is:A/><have:Y>β</have:Y></the:B></the:s></the:b>");

        Expression c = new Expression(
            _(THE._, "c", _(AN._, "s", _(AN._, "uc")))
        );
        assertAnimo(c, "<the:c><the:s><the:C><is:B/><have:Z>γ</have:Z><have:X>αα</have:X></the:C></the:s></the:c>");

    }

    @Test
    public void complex_any_Use() throws EBuilderTerminated, IOException, InterruptedException {
        new Expression(
            _(THE._, "A", _(IS._, "S"), _(HAVE._, "X", text("α")))
        );

        new Expression(
            _(THE._, "B", _(IS._, "A"), _(HAVE._, "Y", text("β")))
        );
        new Expression(
            _(THE._, "B1", _(IS._, "B"), _(HAVE._, "Y", text("ββ")))
        );

        new Expression(
            _(THE._, "C", _(IS._, "B"), _(HAVE._, "Z", text("γ")),  _(HAVE._, "X", text("αα")))
        );
        new Expression(
            _(THE._, "C1", _(IS._, "C"), _(HAVE._, "Z", text("γγ")),  _(HAVE._, "X", text("ααα")))
        );

        new Expression (
            _(THE._, "s", _(ANY._, "S"))
        );

        Expression b = new Expression(
            _(THE._, "b", _(AN._, "s", _(USE._, "B")))
        );
        assertAnimo(b, "<the:b><the:s><the:B><is:A/><have:Y>β</have:Y></the:B></the:s></the:b>");

        Expression c = new Expression(
            _(THE._, "c", _(AN._, "s", _(USE._, "C")))
        );
        assertAnimo(c, "<the:c><the:s><the:C><is:B/><have:Z>γ</have:Z><have:X>αα</have:X></the:C></the:s></the:c>");

    }

    @Test
    public void complex_any_Use_1() throws EBuilderTerminated, IOException, InterruptedException {
        new Expression(
            _(THE._, "A", _(IS._, "S"), _(HAVE._, "X", text("α")))
        );

        new Expression(
            _(THE._, "B", _(IS._, "A"), _(HAVE._, "Y", text("β")))
        );
        new Expression(
            _(THE._, "B1", _(IS._, "B"), _(HAVE._, "Y", text("ββ")))
        );

        new Expression(
            _(THE._, "C", _(IS._, "B"), _(HAVE._, "Z", text("γ")),  _(HAVE._, "X", text("αα")))
        );
        new Expression(
            _(THE._, "C1", _(IS._, "C"), _(HAVE._, "Z", text("γγ")),  _(HAVE._, "X", text("ααα")))
        );

        new Expression (
            _(THE._, "s", _(ANY._, "S"))
        );

        new Expression (
            _(THE._, "ub", _(USE._, "B"))
        );

        new Expression (
            _(THE._, "uc", _(USE._, "C"))
        );

        Expression b = new Expression(
            _(THE._, "b", _(AN._, "s", _(AN._, "ub")))
        );
        assertAnimo(b, "<the:b><the:s><the:B><is:A/><have:Y>β</have:Y></the:B></the:s></the:b>");

        Expression c = new Expression(
            _(THE._, "c", _(AN._, "s", _(AN._, "uc")))
        );
        assertAnimo(c, "<the:c><the:s><the:C><is:B/><have:Z>γ</have:Z><have:X>αα</have:X></the:C></the:s></the:c>");

    }

}
