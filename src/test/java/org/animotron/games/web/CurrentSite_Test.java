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
import org.animotron.expression.JExpression;
import org.animotron.statement.compare.WITH;
import org.animotron.statement.operator.AN;
import org.animotron.statement.operator.THE;
import org.animotron.statement.query.ANY;
import org.animotron.statement.query.GET;
import org.animotron.statement.relation.USE;
import org.junit.Test;

import static org.animotron.expression.JExpression._;
import static org.animotron.expression.JExpression.value;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class CurrentSite_Test extends ATest {

    @Test
    public void test() throws Exception {

        JExpression.__(
                new JExpression(
                        _(THE._, "current-site",
                                _(ANY._, "site",
                                        _(WITH._, "server-name", _(GET._, "host", _(ANY._, "request")))
                                )
                        )
                ),
                new JExpression(
                        _(THE._, "test-service",
                                _(AN._, "service")
                        )
                ),
                new JExpression(
                        _(THE._, "localhost-site",
                                _(AN._, "site"),
                                _(AN._, "server-name", value("localhost"))
                        )
                ),
                new JExpression(
                        _(THE._, "rest",
                                _(ANY._, "service",
                                        _(AN._, "current-site")
                                )
                        )
                ),
                new JExpression(
                        _(THE._, "current-request",
                                _(AN._, "request"),
                                _(AN._, "host", value("localhost"))
                        )
                )
        );

        JExpression s = new JExpression(
            _(AN._, "rest",
                _(USE._, "current-request")
            )
        );

        assertAnimoResult(s, "rest the test-service service.");
    }
}