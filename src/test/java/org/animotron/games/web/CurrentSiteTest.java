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
import org.animotron.statement.relation.HAVE;
import org.animotron.statement.relation.IS;
import org.junit.Test;

import static org.animotron.expression.JExpression._;
import static org.animotron.expression.JExpression.text;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class CurrentSiteTest extends ATest {

    @Test
    public void test() throws Exception {

    	JExpression.__(
                new JExpression(
                        _(THE._, "current-site",
                                _(ANY._, "site",
                                        _(WITH._, "server-name", _(GET._, "host"))
                                )
                        )
                ),
                new JExpression(
                        _(THE._, "test-service",
                                _(IS._, "service"),
                                _(GET._, "server-name"),
                                _(GET._, "host")
                        )
                ),
                new JExpression(
                        _(THE._, "localhost-site",
                                _(IS._, "site"),
                                _(HAVE._, "server-name", text("localhost"))
                        )
                ),
                new JExpression(
                        _(THE._, "rest",
                                _(ANY._, "service",
                                        _(AN._, "current-site")
                                )
                        )
                )
        );

        JExpression s = new JExpression(
            _(AN._, "rest",
                _(HAVE._, "host", text("localhost"))
            )
        );

        assertAnimoResult(s,
            "the rest " +
                "the test-service " +
                "(is service) " +
                    "(have server-name \"localhost\") " +
                    "(have host \"localhost\").");

    }

}