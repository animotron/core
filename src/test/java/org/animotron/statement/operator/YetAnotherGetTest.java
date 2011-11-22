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
import org.animotron.statement.query.GET;
import org.animotron.statement.query.SELF;
import org.animotron.statement.relation.HAVE;
import org.animotron.statement.relation.IC;
import org.animotron.statement.relation.IS;
import org.junit.Test;

import static org.animotron.expression.JExpression._;
import static org.animotron.expression.JExpression.text;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class YetAnotherGetTest extends ATest{

    @Test
    public void get_via_is() throws Exception {

        JExpression.__(
                new JExpression(
                        _(THE._, "B", _(IS._, "A"))
                ),
                new JExpression(
                        _(THE._, "C", _(HAVE._, "B", text("π")))
                )
        );

        JExpression E = new JExpression(
            _(THE._, "E", _(GET._, "A", _(AN._, "C")))
        );
        assertAnimoResult(E, "the E have B \"π\".");

    }

    @Test
    public void get_ic_via_is() throws Exception {

        JExpression.__(
                new JExpression(
                        _(THE._, "B", _(IS._, "A"))
                ),
                new JExpression(
                        _(THE._, "C", _(IC._, "B", text("π")))
                ),
                new JExpression(
                        _(THE._, "D", _(IS._, "C"))
                )
        );

        JExpression E = new JExpression(
            _(THE._, "E", _(GET._, "A", _(AN._, "D")))
        );
        assertAnimoResult(E, "the E have B \"π\".");

    }

    @Test
    public void get_have_via_is() throws Exception {

        JExpression.__(
                new JExpression(
                        _(THE._, "B", _(IS._, "A"))
                ),
                new JExpression(
                        _(THE._, "C", _(HAVE._, "B", text("π")))
                ),
                new JExpression(
                        _(THE._, "D", _(IS._, "C"))
                )
        );

        JExpression E = new JExpression(
            _(THE._, "E", _(GET._, "A", _(AN._, "D")))
        );
        assertAnimoResult(E, "the E have B \"π\".");

    }

    @Test
    public void self_via_is() throws Exception {

        JExpression.__(
                new JExpression(
                        _(THE._, "B", _(IS._, "A"))
                ),
                new JExpression(
                        _(THE._, "C", _(HAVE._, "B", text("π")))
                )
        );

        JExpression E = new JExpression(
            _(THE._, "E", _(IS._, "C"), _(SELF._, "A"))
        );
        assertAnimoResult(E, "the E (is C) (have B \"π\").");

    }

    @Test
    public void self_ic_via_is() throws Exception {

        JExpression.__(
                new JExpression(
                        _(THE._, "B", _(IS._, "A"))
                ),
                new JExpression(
                        _(THE._, "C", _(IC._, "B", text("π")))
                ),
                new JExpression(
                        _(THE._, "D", _(IS._, "C"))
                )
        );

        JExpression E = new JExpression(
            _(THE._, "E", _(IS._, "D"), _(SELF._, "A"))
        );
        assertAnimoResult(E, "the E (is D) (have B \"π\").");

    }

    @Test
    public void self_have_via_is() throws Exception {

        JExpression.__(
                new JExpression(
                        _(THE._, "B", _(IS._, "A"))
                ),
                new JExpression(
                        _(THE._, "C", _(HAVE._, "B", text("π")))
                ),
                new JExpression(
                        _(THE._, "D", _(IS._, "C"))
                )
        );

        JExpression E = new JExpression(
            _(THE._, "E", _(IS._, "D"), _(SELF._, "A"))
        );

        assertAnimoResult(E, "the E (is D) (have B \"π\").");
    }
}