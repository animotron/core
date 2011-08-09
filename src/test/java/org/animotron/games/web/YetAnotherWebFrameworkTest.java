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
import org.animotron.exception.EBuilderTerminated;
import org.animotron.operator.AN;
import org.animotron.operator.THE;
import org.animotron.operator.compare.WITH;
import org.animotron.operator.query.ANY;
import org.animotron.operator.query.GET;
import org.animotron.operator.relation.HAVE;
import org.animotron.operator.relation.IS;
import org.animotron.operator.relation.USE;
import org.junit.Test;

import java.io.IOException;

import static org.animotron.Expression.*;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class YetAnotherWebFrameworkTest extends ATest {

    private void test(Object[]... o) throws EBuilderTerminated, IOException, InterruptedException {

        new Expression (
            _(THE._, "html",
                element("html",
                    element("head",
                        element("title", _(GET._, "title"))
                    ),
                    element("body",
                        _(ANY._, "layout")
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

        Expression s = new Expression(
            _(AN._, "rest",
                _(HAVE._, "uri", text("/")),
                _(HAVE._, "host", text("localhost"))
            )
        );

        assertAnimo(s,  "<the:c93fb6da14911b195f4511c0a060275ef26db4c1803db4d56fee849192330279>" +
                            "<the:rest>" +
                                "<the:root-service>" +
                                    "<is:service/>" +
                                    "<have:uri>/</have:uri>" +
                                    "<the:html>" +
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
                                    "</the:html>" +
                                "</the:root-service>" +
                            "</the:rest>" +
                        "</the:c93fb6da14911b195f4511c0a060275ef26db4c1803db4d56fee849192330279>");

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
    public void test1() throws EBuilderTerminated, IOException, InterruptedException {
        test(
            _(AN._, "root-service",
                _(AN._, "localhost-site")
            )
        );
    }

    @Test
    public void test2() throws EBuilderTerminated, IOException, InterruptedException {
        test(
            _(AN._, "root-service",
                _(ANY._, "site",
                    _(WITH._, "server-name", _(GET._, "host"))
                )
            )
        );
    }

    @Test
    public void test3() throws EBuilderTerminated, IOException, InterruptedException {
        test(
            _(ANY._, "service",
                _(WITH._, "uri", _(GET._, "uri")),
                _(AN._, "localhost-site")
            )
        );
    }

    @Test
    public void test4() throws EBuilderTerminated, IOException, InterruptedException {
        test(
            _(ANY._, "service",
                _(WITH._, "uri", _(GET._, "uri")),
                _(ANY._, "site",
                    _(WITH._, "server-name", _(GET._, "host"))
                )
            )
        );
    }

}
