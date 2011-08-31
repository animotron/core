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
import org.animotron.Expression;
import org.animotron.exception.AnimoException;
import org.animotron.statement.operator.query.ANY;
import org.animotron.statement.relation.IS;
import org.animotron.statement.relation.USE;
import org.junit.Test;

import java.io.IOException;

import static org.animotron.Expression._;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class CrossUseTest extends ATest {

    @Test
    public void cross_use_case() throws IOException, AnimoException {

        new Expression(
            _(THE._, "A", _(IS._, "S"), _(IS._, "X"))
        );

        new Expression(
            _(THE._, "B", _(IS._, "S"), _(IS._, "Y"))
        );

        new Expression(
            _(THE._, "C", _(IS._, "S"), _(IS._, "X"), _(IS._, "Y"))
        );

        Expression s = new Expression (
            _(THE._, "s", _(ANY._, "S"))
        );
        assertAnimoResult(s, "<the:s><the:A><is:S/><is:X/></the:A></the:s>");

        Expression a = new Expression (
            _(THE._, "a", _(ANY._, "S", _(USE._, "X")))
        );
        assertAnimoResult(a, "<the:a><the:A><is:S/><is:X/></the:A></the:a>");

        Expression b = new Expression (
            _(THE._, "b", _(ANY._, "S", _(USE._, "Y")))
        );
        assertAnimoResult(b, "<the:b><the:B><is:S/><is:Y/></the:B></the:b>");

        Expression c = new Expression (
            _(THE._, "c", _(ANY._, "S", _(USE._, "X"), _(USE._, "Y")))
        );
        assertAnimoResult(c, "<the:c><the:C><is:S/><is:X/><is:Y/></the:C></the:c>");

    }

}