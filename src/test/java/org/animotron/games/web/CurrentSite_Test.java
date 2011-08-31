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
package org.animotron.games.web;

import org.animotron.ATest;
import org.animotron.Expression;
import org.animotron.exception.AnimoException;
import org.animotron.statement.operator.AN;
import org.animotron.statement.operator.THE;
import org.animotron.statement.operator.compare.WITH;
import org.animotron.statement.operator.query.ANY;
import org.animotron.statement.operator.query.GET;
import org.animotron.statement.relation.HAVE;
import org.animotron.statement.relation.IS;
import org.animotron.statement.relation.USE;
import org.junit.Test;

import java.io.IOException;

import static org.animotron.Expression._;
import static org.animotron.Expression.text;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class CurrentSite_Test extends ATest {

    @Test
    public void test() throws AnimoException, IOException, InterruptedException {

        new Expression (
            _(THE._, "current-site",
                _(ANY._, "site",
                    _(WITH._, "server-name", _(GET._, "host", _(ANY._, "request")))
                )
            )
        );

        new Expression (
            _(THE._, "test-service",
                _(IS._, "service")
            )
        );

        new Expression (
            _(THE._, "localhost-site",
                _(IS._, "site"),
                _(HAVE._, "server-name", text("localhost"))
            )
        );

        new Expression(
            _(THE._, "rest",
                _(ANY._, "service",
                    _(AN._, "current-site")
                )
            )
        );

        new Expression (
            _(THE._, "current-request",
                _(IS._, "request"),
                _(HAVE._, "host", text("localhost"))
            )
        );

        Expression s = new Expression(
            _(AN._, "rest",
                _(USE._, "current-request")
            )
        );

        assertAnimoResult(s, "<the:3af496a2e8ef6b8f85cf85b360f8ab3775232185e2d2cfe65b1a249ccf8f3cff>" +
                "<the:rest>" +
                "<the:test-service>" +
                "<is:service/>" +
                "</the:test-service>" +
                "</the:rest>" +
                "</the:3af496a2e8ef6b8f85cf85b360f8ab3775232185e2d2cfe65b1a249ccf8f3cff>");

    }

}
