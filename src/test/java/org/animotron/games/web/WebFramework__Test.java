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
import org.animotron.operator.AN;
import org.animotron.operator.THE;
import org.animotron.operator.compare.WITH;
import org.animotron.operator.query.ALL;
import org.animotron.operator.query.ANY;
import org.animotron.operator.query.GET;
import org.animotron.operator.relation.HAVE;
import org.animotron.operator.relation.IS;
import org.animotron.operator.relation.USE;
import org.junit.Ignore;
import org.junit.Test;

import java.io.IOException;

import static org.animotron.Expression.*;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class WebFramework__Test extends ATest {

	@Ignore
	@Test
	public void test() throws IOException, AnimoException {
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
				_(USE._, "theme-concrete-root-layout"),
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
                _(AN._, "html-composition"
		)	)	);

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
                            _(ANY._, "layout")
			)   )	)	)	);

        new Expression(
            _(THE._, "html-head",
                element("head",
                    element("title",
                        _(GET._, "title", _(AN._, "current-service"))
        )   )   )   );

        new Expression(
                _(THE._, "root-layout", _(IS._, "layout"))
        );

    	new Expression(
			_(THE._, "theme-concrete-root-layout", 
				_(IS._, "root-layout"),
				_(HAVE._, "content",
					element("div", attribute("id", "title"),
						_(GET._, "title", _(AN._, "current-service"))),
					element("div", attribute("id", "content"),
                        _(GET._, "content", _(AN._, "current-service")))
		)	)	);

    	new Expression(
			_(THE._, "uuid-c97fd56c-ae81-493a-a508-6fbb8fb12dc", 
				_(IS._, "root-content"),
				_(HAVE._, "title", text("Welcome to Animotron")),
				_(HAVE._, "content", text("Overview"))
		)	);

    	new Expression(
            _(THE._, "service")
        );

        new Expression(
            _(THE._, "current-service",
                _(ANY._, "service",
                    _(WITH._, "uri", _(GET._, "uri", _(AN._, "request")))
                )
            )
        );

        Expression s = new Expression(
            _(THE._, "s", 
        		_(AN._, "current-service",
                    _(AN._, "test-site")
                )
            )
        );
        assertAnimo(s,  "<the:s>" +
                            "<the:current-service>" +
                                "<the:root-service>" +
                                    "<is:service/>" +
                                    "<have:uri>/</have:uri>" +
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
                                                    "</body>" +
                                                "</html>" +
                                            "</have:content>" +
                                        "</the:html-composition>" +
                                    "</the:html>" +
                                "</the:root-service>" +
                            "</the:current-service>" +
                        "</the:s>");
        assertResult(s, "<html>" +
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
