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
import org.animotron.statement.query.GET;
import org.animotron.statement.relation.HAVE;
import org.animotron.statement.relation.IS;
import org.junit.Test;

import java.io.IOException;

import static org.animotron.Expression.*;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class EachTest extends ATest {

    @Test
    public void eachTest() throws IOException, AnimoException {

        new Expression(
            _(THE._, "A", _(IS._, "S"), _(HAVE._, "content", text("α")))
        );

        new Expression(
            _(THE._, "B", _(IS._, "S"), _(HAVE._, "content", text("β")))
        );

        new Expression(
            _(THE._, "C", _(IS._, "S"), _(HAVE._, "content", text("γ")))
        );

        new Expression(
            _(THE._, "P", element("p", _(GET._, "content")))
        );

        Expression s = new Expression (
            _(THE._, "s", _(EACH._, "P", _(ALL._, "S")))
        );
        assertAnimoResult(s, "the s (element p have content \"α\") (element p have content \"β\") (element p have content \"γ\")");

    }

    @Test
    public void eachTest1() throws IOException, AnimoException {

        new Expression(
            _(THE._, "A", _(IS._, "S"), _(IS._, "P"), _(HAVE._, "content", text("α")))
        );

        new Expression(
            _(THE._, "B", _(IS._, "S"), _(IS._, "P"), _(HAVE._, "content", text("β")))
        );

        new Expression(
            _(THE._, "C", _(IS._, "S"), _(HAVE._, "content", text("γ")))
        );

        Expression s = new Expression (
            _(THE._, "s", element("p", _(EACH._, "P", _(ALL._, "S"))))
        );
        assertAnimoResult(s, "the s (element p the A (is A) (is S) (is P) (have content \"α\")) (element p the B (is S) (is P) (have content \"β\"))");

    }

}