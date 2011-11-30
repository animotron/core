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

import static org.animotron.expression.JExpression.*;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class CurrentWebFramework_Test extends ATest {

    @Test
    public void test() throws Exception {

        __(
                new JExpression(
                        _(THE._, "service",
                                _(AN._, "resource")
                        )

                ),
                new JExpression(
                        _(THE._, "html",
                                _(AN._, "mime-type", text("text/html")),
                                _(AN._, "content",
                                        element("html",
                                                element("head",
                                                        element("title", _(GET._, "title", _(ANY._, "service")))
                                                ),
                                                element("body",
                                                        _(ANY._, "layout")
                                                )
                                        )
                                )
                        )
                ),
                new JExpression(
                        _(THE._, "resource-not-found",
                                _(AN._, "not-found-content"),
                                _(AN._, "title", text("Not found")),
                                _(AN._, "content", text("Can't find resource \""), _(GET._, "uri", _(ANY._, "request")), text("\""))
                        )
                ),
                new JExpression(
                        _(THE._, "it-working",
                                _(AN._, "root-content"),
                                _(AN._, "title", text("Welcome to Animo")),
                                _(AN._, "content", text("It is working!"))
                        )
                ),
                new JExpression(
                        _(THE._, "current-site",
                                _(ANY._, "site",
                                        _(WITH._, "server-name", _(GET._, "host", _(ANY._, "request")))
                                )
                        )
                ),
                new JExpression(
                        _(THE._, "localhost-site",
                                _(AN._, "site"),
                                _(AN._, "server-name", text("localhost")),
                                _(USE._, "theme-concrete-root-layout"),
                                _(USE._, "it-working")
                        )
                ),
                new JExpression(
                        _(THE._, "not-found-service",
                                _(AN._, "service"),
                                _(AN._, "not-found"),
                                _(AN._, "html",
                                        _(ANY._, "not-found-content"),
                                        _(USE._, "not-found-layout")
                                )
                        )
                ),
                new JExpression(
                        _(THE._, "root-service",
                                _(AN._, "service"),
                                _(AN._, "root"),
                                _(AN._, "html",
                                        _(ANY._, "root-content"),
                                        _(USE._, "root-layout")
                                )
                        )
                ),
                new JExpression(
                        _(THE._, "not-found-layout",
                                _(AN._, "layout"),
                                element("p", _(GET._, "content", _(ANY._, "service")))
                        )
                ),
                new JExpression(
                        _(THE._, "root-layout",
                                _(AN._, "layout"),
                                element("p", text("The default root layout!"))
                        )
                ),
                new JExpression(
                        _(THE._, "theme-concrete-root-layout",
                                _(AN._, "root-layout"),
                                element("h1", _(GET._, "title", _(ANY._, "service"))),
                                element("p", _(GET._, "content", _(ANY._, "service"))),
                                element("ul",
                                        element("li", text("host: \""), element("strong", _(GET._, "host", _(ANY._, "request"))), text("\"")),
                                        element("li", text("uri: \""), element("strong", _(GET._, "uri", _(ANY._, "request"))), text("\""))
                                )
                        )
                ),
                new JExpression(
                        _(THE._, "rest",
                                _(ANY._, "resource",
                                        _(AN._, "current-site")
                                )
                        )
                ),
                new JExpression(
                        _(THE._, "current-request",
                        		_(AN._, "request"),
                                _(USE._, "root"),
                                _(AN._, "uri", text("/")),
                                _(AN._, "host", text("localhost"))
                        )
                )
        );

        JExpression s = new JExpression(
            _(GET._, "content",
                _(AN._, "rest",
                    _(USE._, "current-request")
                )
            )
        );

        assertAnimoResult(s,
            "have content " +
                "\\html " +
                    "(\\head \\title title \"Welcome to Animo\") " +
                    "(\\body the theme-concrete-root-layout (root-layout) " +
                        "(\\h1 title \"Welcome to Animo\") " +
                        "(\\p content \"It is working!\") " +
                        "(\\ul " +
                            "(\\li \"host: \\\"\" (\\strong host \"localhost\") \"\\\"\") " +
                            "(\\li \"uri: \\\"\" (\\strong uri \"/\") \"\\\"\"))).");


        assertXMLResult(s,
            "<html>" +
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

}
