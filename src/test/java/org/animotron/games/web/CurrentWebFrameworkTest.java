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
import org.animotron.exception.AnimoException;
import org.animotron.statement.compare.WITH;
import org.animotron.statement.operator.AN;
import org.animotron.statement.operator.THE;
import org.animotron.statement.query.ANY;
import org.animotron.statement.query.GET;
import org.animotron.statement.relation.HAVE;
import org.animotron.statement.relation.IS;
import org.animotron.statement.relation.USE;
import org.junit.Test;

import java.io.IOException;

import static org.animotron.expression.JExpression.*;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class CurrentWebFrameworkTest extends ATest {

    @Test
    public void test() throws AnimoException, IOException, InterruptedException {

        new JExpression(
            _(THE._, "service",
                _(IS._, "resource")
            )
        );

        new JExpression(
            _(THE._, "html",
                _(HAVE._, "mime-type", text("text/html")),
                _(HAVE._, "content",
                    element("html",
                        element("head",
                            element("title", _(GET._, "title"))
                        ),
                        element("body",
                            _(ANY._, "layout")
                        )
                    )
                )
            )
        );

        new JExpression(
            _(THE._, "resource-not-found",
                _(IS._, "not-found-content"),
                _(HAVE._, "title", text("Not found")),
                _(HAVE._, "content", text("Can't find resource \""), _(GET._, "uri"), text("\""))
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
            _(THE._, "current-site",
                _(ANY._, "site",
                    _(WITH._, "server-name", _(GET._, "host"))
                )
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
            _(THE._, "not-found-service",
                _(IS._, "service"),
                _(IS._, "not-found"),
                _(AN._, "html",
                    _(ANY._, "not-found-content"),
                    _(USE._, "not-found-layout")
                )
            )
        );

        new JExpression(
            _(THE._, "root-service",
                _(IS._, "service"),
                _(IS._, "root"),
                _(AN._, "html",
                    _(ANY._, "root-content"),
                    _(USE._, "root-layout")
                )
            )
        );

        new JExpression(
            _(THE._, "not-found-layout",
                _(IS._, "layout"),
                element("p",  _(GET._, "content"))
            )
        );

        new JExpression(
            _(THE._, "root-layout",
                _(IS._, "layout"),
                element("p", text("The default root layout!"))
            )
        );

        new JExpression(
            _(THE._, "theme-concrete-root-layout",
                _(IS._, "root-layout"),
                element("h1", _(GET._, "title")),
                element("p", _(GET._, "content")),
                element("ul",
                    element("li", text("host: \""), element("strong" ,_(GET._, "host")), text("\"")),
                    element("li", text("uri: \""), element("strong", _(GET._, "uri")), text("\""))
                )
            )
        );

        new JExpression(
            _(THE._, "rest",
                _(ANY._, "resource",
                    _(AN._, "current-site")
                )
            )
        );

        JExpression s = new JExpression(
            _(GET._, "content",
                _(AN._, "rest",
                    _(USE._, "root"),
                    _(HAVE._, "uri", text("/")),
                    _(HAVE._, "host", text("localhost"))
                )
            )
        );

        assertAnimoResult(s,  "the 6a40e5b412692b64b8e07615e06e442dc05c3c4d44c09faa70e4cd34bf3abf25 " +
                                "have content " +
                                    "\\html " +
                                        "(\\head \\title have title \"Welcome to Animo\") " +
                                        "(\\body the theme-concrete-root-layout (is root-layout) " +
                                            "(\\h1 have title \"Welcome to Animo\") " +
                                            "(\\p have content \"It is working\") " +
                                            "(\\ul " +
                                                "(\\li (\"host:\") (\\strong have host \"localhost\")) " +
                                                "(\\li (\"uri:\") (\\strong have uri \"/\"))))");


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

}
