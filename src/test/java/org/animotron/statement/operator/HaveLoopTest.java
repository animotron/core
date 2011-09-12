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
import org.animotron.statement.query.ANY;
import org.animotron.statement.query.GET;
import org.animotron.statement.relation.HAVE;
import org.animotron.statement.relation.IS;
import org.junit.Ignore;
import org.junit.Test;

import static org.animotron.Expression._;
import static org.animotron.Expression.text;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class HaveLoopTest extends ATest {
	
    @Test
    public void loop_test() throws Exception {

        new Expression(
            _(THE._, "A", _(HAVE._, "B", _(AN._, "C")))
        );

        new Expression(
            _(THE._, "C", _(GET._, "B"))
        );

        Expression s = new Expression(
            _(THE._, "s",
                _(AN._, "A",
                    _(HAVE._, "B", text("test"))
                )
            )
        );
        assertAnimoResult(s, "the s the A have B the C have B \"test\"");
    }

    @Test
    public void loop_test_() throws Exception {

        new Expression(
            _(THE._, "A", _(HAVE._, "B", _(AN._, "C")))
        );

        new Expression(
            _(THE._, "C", _(GET._, "B"))
        );

        Expression s = new Expression(
            _(THE._, "s",
                _(AN._, "A",
                    _(HAVE._, "B", text("test"))
                )
            )
        );
        assertAnimoResult(s, "the s the A have B the C have B \"test\"");
    }

    @Test
    public void loop_test1() throws Exception {

        new Expression(
            _(THE._, "A", _(HAVE._, "B", _(AN._, "C")))
        );

        new Expression(
            _(THE._, "C", _(GET._, "B"))
        );

        new Expression(
            _(THE._, "D",
                _(HAVE._, "B", text("test"))
            )
        );

        Expression s = new Expression(
            _(THE._, "s",
                _(AN._, "A", _(AN._, "D"))
            )
        );
        assertAnimoResult(s, "the s the A have B the C have B \"test\"");
    }

    @Test
    public void loop_test2() throws Exception {

        new Expression(
            _(THE._, "A", _(HAVE._, "B", _(AN._, "C")))
        );

        new Expression(
            _(THE._, "C", _(GET._, "B"))
        );

        new Expression(
            _(THE._, "D",
                _(AN._, "E")
            )
        );

        new Expression(
            _(THE._, "E",
                _(HAVE._, "B", text("test"))
            )
        );

        Expression s = new Expression(
            _(THE._, "s",
                _(AN._, "A", _(AN._, "D"))
            )
        );
        assertAnimoResult(s, "the s the A have B the C have B \"test\"");
    }

    @Test
    public void loop_test3() throws Exception {

        new Expression(
            _(THE._, "A", _(HAVE._, "B", _(AN._, "C")))
        );

        new Expression(
            _(THE._, "C", _(GET._, "B"))
        );

        new Expression(
            _(THE._, "D",
                _(IS._, "d"),
                _(HAVE._, "B", text("test"))
            )
        );

        Expression s = new Expression(
            _(THE._, "s",
                _(AN._, "A", _(ANY._, "d"))
            )
        );
        assertAnimoResult(s, "the s the A have B the C have B \"test\"");
    }

    @Test
    public void loop_get_test() throws Exception {

        new Expression(
            _(THE._, "A", _(HAVE._, "B", _(AN._, "C")))
        );

        new Expression(
            _(THE._, "C", _(GET._, "B"))
        );

        Expression s = new Expression(
            _(THE._, "s",
                _(GET._, "B",
                    _(AN._, "A",
                        _(HAVE._, "B", text("test"))
                    )
                )
            )
        );
        assertAnimoResult(s, "the s have B \"test\"");
    }

    @Test
    @Ignore //because context for get B under the C unclear
    public void loop_get_test1() throws Exception {

        new Expression(
            _(THE._, "A", _(HAVE._, "B", _(AN._, "C")))
        );

        new Expression(
            _(THE._, "C", _(GET._, "B"))
        );

        new Expression(
            _(THE._, "D",
                _(HAVE._, "B", text("test"))
            )
        );

        Expression s = new Expression(
            _(THE._, "s",
                _(GET._, "B",
                    _(AN._, "A", _(AN._, "D"))
                )
            )
        );
        assertAnimoResult(s, "the s have B the C have B \"test\"");
    }

    @Test
    @Ignore //because context for get B under the C unclear
    public void loop_get_test2() throws Exception {

        new Expression(
            _(THE._, "A", _(HAVE._, "B", _(AN._, "C")))
        );

        new Expression(
            _(THE._, "C", _(GET._, "B"))
        );

        new Expression(
            _(THE._, "D",
                _(AN._, "E")
            )
        );

        new Expression(
            _(THE._, "E",
                _(HAVE._, "B", text("test"))
            )
        );

        Expression s = new Expression(
            _(THE._, "s",
                _(GET._, "B",
                    _(AN._, "A", _(AN._, "D"))
                )
            )
        );
        assertAnimoResult(s, "the s have B the C have B \"test\"");
    }

    @Test
    @Ignore //because context for get B under the C unclear
    public void loop_get_test3() throws Exception {

        new Expression(
            _(THE._, "A", _(HAVE._, "B", _(AN._, "C")))
        );

        new Expression(
            _(THE._, "C", _(GET._, "B"))
        );

        new Expression(
            _(THE._, "D",
                _(IS._, "d"),
                _(HAVE._, "B", text("test"))
            )
        );

        Expression s = new Expression(
            _(THE._, "s",
                _(GET._, "B",
                    _(AN._, "A", _(ANY._, "d"))
                )
            )
        );
        assertAnimoResult(s, "the s have B the C have B \"test\"");
    }
}
