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
import org.animotron.statement.query.ALL;
import org.animotron.statement.query.ANY;
import org.animotron.statement.query.GET;
import org.animotron.statement.relation.USE;
import org.junit.Ignore;
import org.junit.Test;

import static org.animotron.expression.JExpression.*;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class WebFramework__Test extends ATest {

	@Test
	@Ignore
	public void test() throws Exception {

        __(

                new JExpression(
                        _(THE._, "request",
                                _(AN._, "uri", text("/")),
                                _(AN._, "method", text("GET")),
                                _(AN._, "host", text("localhost")),
                                _(USE._, "theme-concrete"), //why do we need this two here? theme def @test-site & layout @root-service
                                _(USE._, "root-layout")
                        )
                ),

                new JExpression(
                        _(THE._, "test-site", //site-context
                                _(AN._, "site"),
                                _(AN._, "server-name",
                                        text("animotron.org"),
                                        text("localhost")),
                                _(USE._, "local-cdn"),
                                _(USE._, "theme-concrete-root-layout"),
                                _(USE._, "uuid-c97fd56c-ae81-493a-a508-6fbb8fb12dc")
                        )
                ),

                new JExpression(
                        _(THE._, "root-service",
                                _(AN._, "service"),
                                _(AN._, "uri", text("/")),
                                _(AN._, "html",
                                        _(ANY._, "root-content"),
                                        _(USE._, "root-layout")
                                ))),

                new JExpression(
                        _(THE._, "html",
                                _(AN._, "html-composition"
                                ))),

                new JExpression(
                        _(THE._, "html-composition",
                                _(AN._, "composition"),
                                _(AN._, "content",
                                        element("html",
                                                _(AN._, "html-head",
                                                        _(AN._, "css",
                                                                _(ALL._, "css")),
                                                        _(AN._, "script",
                                                                _(ALL._, "script"))
                                                ),
                                                element("body",
                                                        _(ANY._, "layout")
                                                ))))),

                new JExpression(
                        _(THE._, "html-head",
                                element("head",
                                        element("title",
                                                _(GET._, "title", _(AN._, "current-service"))
                                        )))),

                new JExpression(
                        _(THE._, "root-layout", _(AN._, "layout"))
                ),

                new JExpression(
                        _(THE._, "theme-concrete-root-layout",
                                _(AN._, "root-layout"),
                                _(AN._, "content",
                                        element("div", attribute("id", "title"),
                                                _(GET._, "title", _(AN._, "current-service"))),
                                        element("div", attribute("id", "content"),
                                                _(GET._, "content", _(AN._, "current-service")))
                                ))),

                new JExpression(
                        _(THE._, "uuid-c97fd56c-ae81-493a-a508-6fbb8fb12dc",
                                _(AN._, "root-content"),
                                _(AN._, "title", text("Welcome to Animotron")),
                                _(AN._, "content", text("Overview"))
                        )),

                new JExpression(
                        _(THE._, "service")
                ),

                new JExpression(
                        _(THE._, "current-service",
                                _(ANY._, "service",
                                        _(WITH._, "uri", _(GET._, "uri", _(AN._, "request")))
                                )
                        )
                )

        );

        JExpression s = new JExpression(
            _(THE._, "s", 
        		_(AN._, "current-service",
                    _(AN._, "test-site")
                )
            )
        );

        assertAnimoResult(s,
            "the s " +
                "the current-service " +
                    "the root-service " +
                        "(service) " +
                        "(uri \"/\") " +
                        "(the html " +
                            "the html-composition " +
                                "(composition) " +
                                "(content " +
                                    "\\html " +
                                        "(the html-head \\head \\title title \"Welcome to Animotron\") " +
                                        "(\\body the theme-concrete-root-layout " +
                                            "(root-layout) " +
                                            "(content " +
                                                "(\\div (@id \"title\") (title \"Welcome to Animotron\")) " +
                                                "(\\div (@id \"content\") (content \"Overview\")))))).");

        assertXMLResult(s,
            "<html>" +
                "<head>" +
                    "<title>Welcome to Animotron</title>" +
                "</head>" +
                "<body>" +
                    "<div id=\"title\">Welcome to Animotron</div>" +
                    "<div id=\"content\">Overview</div>" +
                "</body>" +
            "</html>");

	}

}
