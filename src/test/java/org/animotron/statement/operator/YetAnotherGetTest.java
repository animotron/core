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
import org.animotron.statement.relation.SHALL;
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
                        _(THE._, "B", _(AN._, "A"))
                ),
                new JExpression(
                        _(THE._, "C", _(AN._, "B", text("π")))
                )
        );

        JExpression E = new JExpression(
            _(THE._, "E", _(GET._, "A", _(AN._, "C")))
        );
        assertAnimoResult(E, "the E B \"π\".");

    }

    @Test
    public void get_shall_via_is() throws Exception {

        JExpression.__(
                new JExpression(
                        _(THE._, "B", _(AN._, "A"))
                ),
                new JExpression(
                        _(THE._, "C", _(SHALL._, "B", text("π")))
                ),
                new JExpression(
                        _(THE._, "D", _(AN._, "C"))
                )
        );

        JExpression E = new JExpression(
            _(THE._, "E", _(GET._, "A", _(AN._, "D")))
        );
        //XXX: assertAnimoResult(E, "the E B \"π\".");
        assertAnimoResult(E, "the E shall B \"π\".");
    }

    @Test
    public void get_have_via_is() throws Exception {

        JExpression.__(
                new JExpression(
                        _(THE._, "B", _(AN._, "A"))
                ),
                new JExpression(
                        _(THE._, "C", _(AN._, "B", text("π")))
                ),
                new JExpression(
                        _(THE._, "D", _(AN._, "C"))
                )
        );

        JExpression E = new JExpression(
            _(THE._, "E", _(GET._, "A", _(AN._, "D")))
        );
        assertAnimoResult(E, "the E B \"π\".");

    }

    @Test
    public void self_via_is() throws Exception {

        JExpression.__(
                new JExpression(
                        _(THE._, "B", _(AN._, "A"))
                ),
                new JExpression(
                        _(THE._, "C", _(AN._, "B", text("π")))
                )
        );

        JExpression E = new JExpression(
            _(THE._, "E", _(AN._, "C"), _(SELF._, "A"))
        );
        assertAnimoResult(E, "the E (C) (B \"π\").");

    }

    @Test
    public void self_ic_via_is() throws Exception {

        JExpression.__(
                new JExpression(
                        _(THE._, "B", _(AN._, "A"))
                ),
                new JExpression(
                        _(THE._, "C", _(SHALL._, "B", text("π")))
                ),
                new JExpression(
                        _(THE._, "D", _(AN._, "C"))
                )
        );

        JExpression E = new JExpression(
            _(THE._, "E", _(AN._, "D"), _(SELF._, "A"))
        );
        assertAnimoResult(E, "the E (D) (B \"π\").");

    }

    @Test
    public void self_have_via_is() throws Exception {

        JExpression.__(
                new JExpression(
                        _(THE._, "B", _(AN._, "A"))
                ),
                new JExpression(
                        _(THE._, "C", _(AN._, "B", text("π")))
                ),
                new JExpression(
                        _(THE._, "D", _(AN._, "C"))
                )
        );

        JExpression E = new JExpression(
            _(THE._, "E", _(AN._, "D"), _(SELF._, "A"))
        );

        assertAnimoResult(E, "the E (D) (B \"π\").");
    }
}