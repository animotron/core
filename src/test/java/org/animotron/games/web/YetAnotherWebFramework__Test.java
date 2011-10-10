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
import org.animotron.statement.relation.USE;
import org.junit.Test;

import static org.animotron.expression.JExpression.*;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class YetAnotherWebFramework__Test extends ATest {

    private void test(Object[]... o) throws Exception {

        new JExpression(
            _(THE._, "html",
                element("html",
                    element("head",
                        element("title", _(GET._, "title", _(AN._, "current-service")))
                    ),
                    element("body",
                        _(ANY._, "layout")
                    )
                )
            )
        );

        new JExpression(
            _(THE._, "it-working",
                _(IS._, "root-content"),
                _(HAVE._, "title", text("Welcome to Animo")),
                _(HAVE._, "content", text("It is working!"))
            )
        );

        new JExpression(
            _(THE._, "localhost-site",
                _(IS._, "site"),
                _(HAVE._, "server-name", text("localhost")),
                _(USE._, "theme-concrete-root-layout"),
                _(USE._, "it-working")
            )
        );

        new JExpression(
            _(THE._, "root-service",
                _(IS._, "service"),
                _(HAVE._, "uri", text("/")),
                _(AN._, "html",
                    _(ANY._, "root-content"),
                    _(USE._, "root-layout")
                )
            )
        );

        new JExpression(
            _(THE._, "root-layout",
                _(IS._, "layout"),
                element("p", text("Default layout"))
            )
        );

        new JExpression(
            _(THE._, "theme-concrete-root-layout",
                _(IS._, "root-layout"),
                element("h1", _(GET._, "title", _(AN._, "current-service"))),
                element("p", _(GET._, "content", _(AN._, "current-service"))),
                element("ul",
                    element("li", text("host: \""), element("strong", _(GET._, "host", _(ANY._, "request"))), text("\"")),
                    element("li", text("uri: \""), element("strong", _(GET._, "uri", _(ANY._, "request"))), text("\""))
                )
            )
        );

        new JExpression(
            _(THE._, "current-service",
                _(ANY._, "service",
                        _(WITH._, "uri", _(GET._, "uri", _(ANY._, "request")))
                )
            )
        );

        new JExpression(
            _(THE._, "current-request",
                _(IS._, "request"),
                _(HAVE._, "uri", text("/")),
                _(HAVE._, "host", text("localhost"))
            )
        );

        new JExpression(
            _(THE._, "rest",
                o
            )
        );

        JExpression s = new JExpression(
            _(AN._, "rest",
                _(USE._, "current-request")
            )
        );

        assertAnimoResult(s,  "the 8f0dbda70f443506a24db4cca022992045434baceb63576087a2f528ee121d23 " +
                                "the rest " +
                                    "the root-service " +
                                    "(is service) " +
                                    "(have uri \"/\") " +
                                    "(the html " +
                                        "\\html " +
                                            "(\\head \\title have title \"Welcome to Animo\") " +
                                            "(\\body the theme-concrete-root-layout (is root-layout) " +
                                                "(\\h1 have title \"Welcome to Animo\") " +
                                                "(\\p have content \"It is working!\") " +
                                                "(\\ul " +
                                                    "(\\li (\"host: \\\"\") (\\strong have host \"localhost\") (\"\\\"\")) " +
                                                    "(\\li (\"uri: \\\"\") (\\strong have uri \"/\") (\"\\\"\")))))");


        assertXMLResult(s,  "<html>" +
                                "<head>" +
                                    "<title>Welcome to Animo</title>" +
                                "</head>" +
                                    "<body>" +
                                        "<h1>Welcome to Animo</h1>" +
                                        "<p>It is working!</p>" +
                                        "<ul>" +
                                            "<li>host: \"<strong>localhost</strong>\"</li>" +
                                            "<li>uri: \"<strong>/</strong>\"</li>" +
                                        "</ul>" +
                                    "</body>" +
                                "</html>");

    }

    @Test
    public void test1() throws Exception {
        test(
            _(AN._, "root-service",
                _(AN._, "localhost-site")
            )
        );
    }

    @Test
    public void test2() throws Exception {
        test(
            _(AN._, "root-service",
                _(ANY._, "site",
                    _(WITH._, "server-name", _(GET._, "host", _(ANY._, "request")))
                )
            )
        );
    }

    @Test
    public void test3() throws Exception {
        test(
            _(AN._, "current-service",
                _(AN._, "localhost-site")
            )
        );
    }

    @Test
    public void test4() throws Exception {
        test(
            _(AN._, "current-service",
                _(ANY._, "site",
                    _(WITH._, "server-name", _(GET._, "host", _(ANY._, "request")))
                )
            )
        );
    }

}
