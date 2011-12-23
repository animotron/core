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
public class CurrentWebFrameworkTest extends ATest {

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
                    _(AN._, "mime-type", value("value/html")),
                    element("html",
                            element("head",
                                    element("title", _(GET._, "title"))
                            ),
                            element("body",
                                    _(ANY._, "layout")
                            )
                    )
                )
            ),
            new JExpression(
                _(THE._, "resource-not-found",
                    _(AN._, "service"),
                    _(AN._, "not-found"),
                    _(AN._, "html",
                        _(AN._, "title", value("Not found")),
                        _(AN._, "content", value("Can't find resource \""), _(GET._, "uri"), value("\""))
                    )
                )
            ),
            new JExpression(
                _(THE._, "is-it-working",
                    _(AN._, "service"),
                    _(AN._, "root"),
                    _(AN._, "localhost"),
                    _(AN._, "html",
                        _(AN._, "title", value("Welcome to Localhost")),
                        _(AN._, "content", value("Is it working?"))
                    )
                )
            ),
            new JExpression(
                _(THE._, "localhost-site",
                    _(AN._, "site"),
                    _(AN._, "server-name", value("localhost")),
                    _(USE._, "theme-concrete"),
                    _(USE._, "localhost")
                )
            ),
            new JExpression(
                _(THE._, "it-working",
                    _(AN._, "service"),
                    _(AN._, "root"),
                    _(AN._, "animo"),
                    _(AN._, "html",
                            _(AN._, "title", value("Welcome to Animo")),
                            _(AN._, "content", value("It is working!"))
                    )
                )
            ),
            new JExpression(
                _(THE._, "animo-site",
                    _(AN._, "site"),
                    _(AN._, "server-name", value("animo")),
                    _(USE._, "theme-concrete"),
                    _(USE._, "animo")
                )
            ),
            new JExpression(
                _(THE._, "not-found-layout",
                    _(AN._, "layout"),
                    _(AN._, "not-found"),
                    element("p", _(GET._, "content"))
                )
            ),
            new JExpression(
                _(THE._, "theme-concrete-root-layout",
                    _(AN._, "theme-concrete"),
                    _(AN._, "layout"),
                    _(AN._, "root"),
                    element("h1", _(GET._, "title")),
                    element("p", _(GET._, "content")),
                    element("ul",
                        element("li", value("host: \""), element("strong", _(GET._, "host")), value("\"")),
                        element("li", value("uri: \""), element("strong", _(GET._, "uri")), value("\""))
                    )
                )
            ),
            new JExpression(
                _(THE._, "theme-concrete-not-found-layout",
                    _(AN._, "theme-concrete"),
                    _(AN._, "layout"),
                    _(AN._, "not-found"),
                    element("h1", _(GET._, "title")),
                    element("p", _(GET._, "content"))
                )
            ),
            new JExpression(
        		_(THE._, "rest",
    				_(ANY._, "site",
						_(WITH._, "server-name", _(GET._, "host"))
					),
					_(ANY._, "resource")
				)
            )
        );

        JExpression s1 = new JExpression(
            _(AN._, "rest",
                _(USE._, "root"),
                _(AN._, "uri", value("/")),
                _(AN._, "host", value("localhost"))
            )
        );

        assertAnimoResult(s1,
            "rest " +
                "(the localhost-site " +
                    "(site) " +
                    "(server-name) " +
                    "(use theme-concrete) " +
                    "(use localhost)) " +
                "(the is-it-working " +
                    "(service resource) " +
                    "(root) " +
                    "(localhost) " +
                    "(html " +
                        "(mime-type) " +
                        "(\\html " +
                            "(\\head \\title title \"Welcome to Localhost\") " +
                            "(\\body " +
                                "the theme-concrete-root-layout " +
                                    "(theme-concrete) " +
                                    "(layout) " +
                                    "(root) " +
                                    "(\\h1 title \"Welcome to Localhost\") " +
                                    "(\\p content \"Is it working?\") " +
                                    "(\\ul " +
                                        "(\\li \"host: \\\"\" (\\strong host \"localhost\") \"\\\"\") " +
                                        "(\\li \"uri: \\\"\" (\\strong uri \"/\") \"\\\"\")))))).");

        assertXMLResult(s1,
            "<html>" +
                "<head>" +
                    "<title>Welcome to Localhost</title>" +
                "</head>" +
                "<body>" +
                    "<h1>Welcome to Localhost</h1>" +
                    "<p>Is it working?</p>" +
                    "<ul>" +
                        "<li>host: \"<strong>localhost</strong>\"</li>" +
                        "<li>uri: \"<strong>/</strong>\"</li>" +
                    "</ul>" +
                "</body>" +
            "</html>");

        JExpression s2 = new JExpression(
            _(AN._, "rest",
                _(USE._, "root"),
                _(AN._, "uri", value("/")),
                _(AN._, "host", value("animo"))
            )
        );

        assertAnimoResult(s2,
            "rest " +
                "(the animo-site " +
                    "(site) " +
                    "(server-name) " +
                    "(use theme-concrete) " +
                    "(use animo)) " +
                "(the it-working " +
                    "(service resource) " +
                    "(root) " +
                    "(animo) " +
                    "(html " +
                        "(mime-type) " +
                        "(\\html " +
                            "(\\head \\title title \"Welcome to Animo\") " +
                            "(\\body " +
                                "the theme-concrete-root-layout " +
                                    "(theme-concrete) " +
                                    "(layout) " +
                                    "(root) " +
                                    "(\\h1 title \"Welcome to Animo\") " +
                                    "(\\p content \"It is working!\") " +
                                    "(\\ul " +
                                        "(\\li \"host: \\\"\" (\\strong host \"animo\") \"\\\"\") " +
                                        "(\\li \"uri: \\\"\" (\\strong uri \"/\") \"\\\"\")))))).");

        assertXMLResult(s2,
            "<html>" +
                "<head>" +
                    "<title>Welcome to Animo</title>" +
                "</head>" +
                "<body>" +
                    "<h1>Welcome to Animo</h1>" +
                    "<p>It is working!</p>" +
                    "<ul>" +
                        "<li>host: \"<strong>animo</strong>\"</li>" +
                        "<li>uri: \"<strong>/</strong>\"</li>" +
                    "</ul>" +
                "</body>" +
            "</html>");

        JExpression s3 = new JExpression(
            _(AN._, "rest",
                _(USE._, "not-found"),
                _(AN._, "uri", value("/foo")),
                _(AN._, "host", value("localhost"))
            )
        );

        assertAnimoResult(s3,
            "rest " +
                "(the localhost-site " +
                    "(site) " +
                    "(server-name) " +
                    "(use theme-concrete) " +
                    "(use localhost)) " +
                "(the resource-not-found " +
                    "(service resource) " +
                    "(not-found) " +
                    "(localhost) " +
                    "(html " +
                        "(mime-type) " +
                        "(\\html " +
                            "(\\head \\title title \"Not found\") " +
                            "(\\body " +
                                "the theme-concrete-not-found-layout " +
                                    "(theme-concrete) " +
                                    "(layout) " +
                                    "(not-found) " +
                                    "(\\h1 title \"Not found\") " +
                                    "(\\p content \"Can't find resource \\\"\" (uri \"/foo\") \"\\\"\"))))).");

        assertXMLResult(s3,
                "<html>" +
                    "<head>" +
                        "<title>Not found</title>" +
                    "</head>" +
                    "<body>" +
                        "<h1>Not found</h1>" +
                        "<p>Can't find resource \"/foo\"</p>" +
                    "</body>" +
                "</html>");

        JExpression s4 = new JExpression(
            _(AN._, "rest",
                _(USE._, "not-found"),
                _(AN._, "uri", value("/bar")),
                _(AN._, "host", value("animo"))
            )
        );

        assertAnimoResult(s4,
            "rest " +
                "(the animo-site " +
                    "(site) " +
                    "(server-name) " +
                    "(use theme-concrete) " +
                    "(use animo)) " +
                "(the resource-not-found " +
                    "(service resource) " +
                    "(not-found) " +
                    "(animo) " +
                    "(localhost) " +
                    "(html " +
                    "(mime-type) " +
                    "(\\html " +
                        "(\\head \\title title \"Not found\") " +
                        "(\\body " +
                            "the theme-concrete-not-found-layout " +
                                "(theme-concrete) " +
                                "(layout) " +
                                "(not-found) " +
                                "(\\h1 title \"Not found\") " +
                                "(\\p content \"Can't find resource \\\"\" (uri \"/bar\") \"\\\"\"))))).");

        assertXMLResult(s4,
            "<html>" +
                "<head>" +
                    "<title>Not found</title>" +
                "</head>" +
                "<body>" +
                    "<h1>Not found</h1>" +
                    "<p>Can't find resource \"/bar\"</p>" +
                "</body>" +
            "</html>");

    }

}