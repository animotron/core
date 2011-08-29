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
import org.animotron.statement.operator.relation.HAVE;
import org.animotron.statement.operator.relation.IS;
import org.animotron.statement.operator.relation.USE;
import org.junit.Test;

import java.io.IOException;

import static org.animotron.Expression.*;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class ModifiedYetAnotherWebFrameworkTest extends ATest {

    private void test(Object[]... o) throws AnimoException, IOException, InterruptedException {

        new Expression(
            _(THE._, "service",
                _(IS._, "resource")
            )

        );

        new Expression (
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

        new Expression (
            _(THE._, "it-working",
                _(IS._, "root-content"),
                _(HAVE._, "title", text("Welcome to Animo")),
                _(HAVE._, "content", text("It is working!"))
            )
        );

        new Expression (
            _(THE._, "localhost-site",
                _(IS._, "site"),
                _(HAVE._, "server-name", text("localhost")),
                _(USE._, "theme-concrete-root-layout"),
                _(USE._, "it-working")
            )
        );

        new Expression (
            _(THE._, "root-service",
                _(IS._, "service"),
                _(HAVE._, "uri", text("/")),
                _(AN._, "html",
                        _(ANY._, "root-content"),
                        _(USE._, "root-layout")
                )
            )
        );

        new Expression (
            _(THE._, "root-layout",
                _(IS._, "layout"),
                element("p", text("Default layout"))
            )
        );

        new Expression (
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

        new Expression(
            _(THE._, "rest",
                o
            )
        );

        Expression m = new Expression(
            _(GET._, "mime-type",
                _(AN._, "rest",
                    _(HAVE._, "uri", text("/")),
                    _(HAVE._, "host", text("localhost"))
                )
            )
        );

        assertString(m, "text/html");


        Expression s = new Expression(
            _(GET._, "content",
                _(AN._, "rest",
                    _(HAVE._, "uri", text("/")),
                    _(HAVE._, "host", text("localhost"))
                )
            )
        );

        assertAnimo(s,  "<the:081ede81178788230418466a8f32ec0fa11b6a971fc7b914e204848a6262509c>" +
                            "<have:content>" +
                                "<html>" +
                                    "<head>" +
                                        "<title><have:title>Welcome to Animo</have:title></title>" +
                                    "</head>" +
                                    "<body>" +
                                        "<the:theme-concrete-root-layout>" +
                                            "<is:root-layout/>" +
                                            "<h1><have:title>Welcome to Animo</have:title></h1>" +
                                            "<p><have:content>It is working!</have:content></p>" +
                                            "<ul>" +
                                                "<li>host: \"<strong><have:host>localhost</have:host></strong>\"</li>" +
                                                "<li>uri: \"<strong><have:uri>/</have:uri></strong>\"</li>" +
                                            "</ul>" +
                                        "</the:theme-concrete-root-layout>" +
                                    "</body>" +
                                "</html>" +
                            "</have:content>" +
                        "</the:081ede81178788230418466a8f32ec0fa11b6a971fc7b914e204848a6262509c>");

        assertResult(s, "<html>" +
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
    public void test1() throws AnimoException, IOException, InterruptedException {
        test(
            _(AN._, "root-service",
                _(AN._, "localhost-site")
            )
        );
    }

    @Test
    public void test2() throws AnimoException, IOException, InterruptedException {
        test(
            _(AN._, "root-service",
                _(ANY._, "site",
                    _(WITH._, "server-name", _(GET._, "host"))
                )
            )
        );
    }

    @Test
    public void test3() throws AnimoException, IOException, InterruptedException {
        test(
            _(ANY._, "service",
                _(WITH._, "uri", _(GET._, "uri")),
                _(AN._, "localhost-site")
            )
        );
    }

    @Test
    public void test4() throws AnimoException, IOException, InterruptedException {
        test(
            _(ANY._, "service",
                _(WITH._, "uri", _(GET._, "uri")),
                _(ANY._, "site",
                    _(WITH._, "server-name", _(GET._, "host"))
                )
            )
        );
    }

    @Test
    public void test5() throws AnimoException, IOException, InterruptedException {
        test(
            _(ANY._, "resource",
                _(WITH._, "uri", _(GET._, "uri")),
                _(ANY._, "site",
                    _(WITH._, "server-name", _(GET._, "host"))
                )
            )
        );
    }

}
