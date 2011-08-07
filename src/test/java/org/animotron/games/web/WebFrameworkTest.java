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

import static org.animotron.Expression.*;

import org.animotron.ATest;
import org.animotron.Expression;
import org.animotron.exception.EBuilderTerminated;
import org.animotron.operator.AN;
import org.animotron.operator.THE;
import org.animotron.operator.compare.WITH;
import org.animotron.operator.query.*;
import org.animotron.operator.relation.*;
import org.junit.Test;

import java.io.IOException;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class WebFrameworkTest extends ATest {

	@Test
	public void test() throws EBuilderTerminated, IOException, InterruptedException {
    	new Expression(
			_(THE._, "request", 
				_(HAVE._, "uri", text("/")),
				_(HAVE._, "method", text("GET")),
				_(HAVE._, "host", text("localhost")),
				_(USE._, "theme-concrete"), //why do we need this two here? theme def @test-site & layout @root-service
				_(USE._, "root-layout")
			)
		);

    	new Expression(
			_(THE._, "test-site", //site-context 
				_(IS._, "site"),
				_(HAVE._, "server-name", 
					text("animotron.org"), 
					text("localhost")),
				_(USE._, "local-cdn"),
				_(USE._, "theme-concrete"),
				_(USE._, "uuid-c97fd56c-ae81-493a-a508-6fbb8fb12dc")
			)
		);

    	new Expression(
			_(THE._, "root-service", 
				_(IS._, "service"),
				_(HAVE._, "uri", text("/")),
				_(AN._, "html",
					_(ANY._, "root-content"),
					_(USE._, "root-layout")
		)	)	);

    	new Expression(
			_(THE._, "html", 
                _(AN._, "html-composition",
//                  _(ANY._, "layout"     TODO Don't working
                    _(AN._, "theme-concrete-root-layout" //TODO fix it (don't working too)
		)	)	)	);

    	new Expression(
			_(THE._, "html-composition", 
				_(IS._, "composition"),
                _(HAVE._, "content",
                    element("html",
                        _(AN._, "html-head",
                            _(HAVE._, "css",
                                _(ALL._, "css")),
                            _(HAVE._, "script",
                                _(ALL._, "script"))
                        ),
                        element("body",
                            _(GET._, "content"))
			)	)	)	);

        new Expression(
            _(THE._, "html-head",
                element("head",
                    element("title",
                        _(GET._, "title")
        )   )   )   );

        new Expression(
                _(THE._, "root-layout", _(IS._, "layout"))
        );

    	new Expression(
			_(THE._, "theme-concrete-root-layout", 
				_(IS._, "root-layout"),
				_(HAVE._, "content",
					element("div", attribute("id", "title"),
						_(GET._, "title")),
					element("div", attribute("id", "content"),
                        _(GET._, "content"))
		)	)	);

    	new Expression(
			_(THE._, "uuid-c97fd56c-ae81-493a-a508-6fbb8fb12dc", 
				_(IS._, "root-content"),
				_(HAVE._, "title", text("Welcome to Animotron")),
				_(HAVE._, "content", text("Overview"))
		)	);

    	new Expression(
            _(THE._, "service", _(ANY._, "service", _(WITH._, "uri", _(GET._, "uri"))))
        );

        Expression s = new Expression(
            _(THE._, "s", _(AN._, "service", _(AN._, "request"), _(AN._, "test-site")))
        );
        assertAnimo(s,  "<the:s>" +
                            "<the:service>" +
                                "<the:root-service>" +
                                    "<is:root-service/>" +
                                    "<the:html>" +
                                        "<the:html-composition>" +
                                            "<is:composition/>" +
                                            "<have:content>" +
                                                "<html>" +
                                                    "<the:html-head>" +
                                                    "<head>" +
                                                        "<title><have:title>Welcome to Animotron</have:title></title>" +
                                                    "</head>" +
                                                    "</the:html-head>" +
                                                    "<body>" +
                                                        "<have:content>" +
                                                            "<the:theme-concrete-root-layout>" +
                                                                "<is:root-layout/>" +
                                                                "<have:content>" +
                                                                    "<div id=\"title\">" +
                                                                        "<have:title>Welcome to Animotron</have:title>" +
                                                                    "</div>" +
                                                                    "<div id=\"content\">" +
                                                                        "<have:content>Overview</have:content>" +
                                                                    "</div>" +
                                                                "</have:content>" +
                                                            "</the:theme-concrete-root-layout>" +
                                                        "</have:content>" +
                                                    "</body>" +
                                                "</html>" +
                                            "</have:content>" +
                                        "</the:html-composition>" +
                                    "</the:html>" +
                                "</the:root-service>" +
                            "</the:service>" +
                        "</the:s>");

	}

}
