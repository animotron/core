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
import org.animotron.statement.query.ALL;
import org.animotron.statement.query.ANY;
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
public class UseTest extends ATest {

    @Test
    public void any_use() throws IOException, AnimoException {
        new Expression(
            _(THE._, "A", _(IS._, "X"))
        );

        Expression x = new Expression(
            _(THE._, "x", _(ANY._, "X", _(USE._, "Y")))
        );
        //assertXMLResult(x, "<the:x><the:A><is:X/></the:A></the:x>");
        assertAnimoResult(x, "the x the A is X");
    }

    @Test
    public void an_any_use() throws IOException, AnimoException {
        new Expression(
            _(THE._, "A", _(IS._, "X"))
        );

        new Expression(
            _(THE._, "q", _(ANY._, "X"))
        );

        Expression x = new Expression(
            _(THE._, "x", _(AN._, "q", _(USE._, "Y")))
        );
        //assertXMLResult(x, "<the:x><the:q><the:A><is:X/></the:A></the:q></the:x>");
        assertAnimoResult(x, "the x the q the A is X");
    }

    @Test
    public void all_use() throws IOException, AnimoException {
        new Expression(
            _(THE._, "A", _(IS._, "X"))
        );

        new Expression(
            _(THE._, "B", _(IS._, "X"))
        );

        Expression x = new Expression(
            _(THE._, "x", _(ALL._, "X", _(USE._, "Y")))
        );
        //assertXMLResult(x, "<the:x><the:A><is:X/></the:A><the:B><is:X/></the:B></the:x>");
        assertAnimoResult(x, "the x the A is X the B is X");
    }

    @Test
    public void an_all_use() throws IOException, AnimoException {
        new Expression(
            _(THE._, "A", _(IS._, "X"))
        );

        new Expression(
            _(THE._, "B", _(IS._, "X"))
        );

        new Expression(
            _(THE._, "q", _(ALL._, "X"))
        );

        Expression x = new Expression(
            _(THE._, "x", _(AN._, "q", _(USE._, "Y")))
        );
        //assertXMLResult(x, "<the:x><the:q><the:A><is:X/></the:A><the:B><is:X/></the:B></the:q></the:x>");
        assertAnimoResult(x, "the x the q the A is X the B is X");
    }
}