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
import org.animotron.statement.compare.WITH;
import org.animotron.statement.operator.AN;
import org.animotron.statement.operator.THE;
import org.animotron.statement.query.ALL;
import org.animotron.statement.query.ANY;
import org.animotron.statement.query.GET;
import org.animotron.statement.relation.HAVE;
import org.animotron.statement.relation.IS;
import org.animotron.statement.relation.USE;
import org.junit.Test;

import java.io.IOException;

import static org.animotron.Expression.*;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class WebFrameworkTest extends ATest {

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
            _(THE._, "service")
        );

        Expression s = new Expression(
            _(THE._, "s", 
        		_(ANY._, "service",
    				_(WITH._, "uri", _(GET._, "uri", _(AN._, "request"))),
                    _(AN._, "test-site")
                )
            )
        );

        assertAnimoResult(s,  "the s " +
                                "the current-service " +
                                    "the root-service " +
                                        "(is service) " +
                                        "(haver uti \"/\") " +
                                        "(the html " +
                                            "(is composition) " +
                                            "(have content " +
                                                "element html " +
                                                    "(the html-head element head element title have title \"Welcome to Animotron\") " +
                                                    "(element body the theme-concrete-root-layout " +
                                                        "(is root-layout) " +
                                                        "(have content " +
                                                            "(element div (attribute id \"title\") (have title \"Welcom to Animotron\"))" +
                                                            "(element div (attribute id \"content\") (have title \"Owervie\"))))))");

        assertXMLResult(s,  "<html>" +
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
