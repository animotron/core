/*
 *  Copyright (C) 2011-2012 The Animo Project
 *  http://animotron.org
 *  	
 *  This file is part of Animotron.
 *  
 *  Animotron is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU Affero General Public License as 
 *  published by the Free Software Foundation, either version 3 of 
 *  the License, or (at your option) any later version.
 *  
 *  Animotron is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Affero General Public License for more details.
 *  
 *  You should have received a copy of 
 *  the GNU Affero General Public License along with Animotron.  
 *  If not, see <http://www.gnu.org/licenses/>.
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

import static org.animotron.expression.AnimoExpression.__;
import static org.animotron.expression.JExpression._;
import static org.animotron.expression.JExpression.element;
import static org.animotron.expression.JExpression.value;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class WebFrameworkTest extends ATest {

    @Test
    public void test_00() throws Exception {

        __(
            new JExpression(
                _(THE._, "service",
                    _(AN._, "resource")
                )
            ),
            new JExpression(
                _(THE._, "html",
                    _(AN._, "mime-type", value("value/html")),
                    _(AN._, "page",
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
            ),
            new JExpression(
                _(THE._, "resource-not-found",
                    _(AN._, "not-found-content"),
                    _(AN._, "title", value("Not found")),
                    _(AN._, "content", value("Can't find resource \""), _(GET._, "uri"), value("\""))
                )
            ),
            new JExpression(
                _(THE._, "it-working",
                    _(AN._, "root-content"),
                    _(AN._, "title", value("Welcome to Animo")),
                    _(AN._, "content", value("It is working!"))
                )
            ),
            new JExpression(
                _(THE._, "localhost-site",
                    _(AN._, "site"),
                    _(AN._, "server-name", value("localhost")),
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
                    element("p", _(GET._, "content"))
                )
            ),
            new JExpression(
                _(THE._, "root-layout",
                    _(AN._, "layout")
                    //,
                    //element("p", value("The default root layout!"))
                )
            ),
            new JExpression(
                _(THE._, "theme-concrete-root-layout",
                    _(AN._, "root-layout"),
                    element("h1", _(GET._, "title")),
                    element("p", _(GET._, "content")),
                    element("ul",
                        element("li", value("host: \""), element("strong", _(GET._, "host")), value("\"")),
                        element("li", value("uri: \""), element("strong", _(GET._, "uri")), value("\""))
                    )
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

        JExpression s = new JExpression(
            _(GET._, "page",
                _(AN._, "rest",
                    _(USE._, "root"),
                    _(AN._, "uri", value("/")),
                    _(AN._, "host", value("localhost"))
                )
            )
        );

        assertAnimoResult(s,
            "page " +
                "\\html " +
                    "(\\head \\title title \"Welcome to Animo\") " +
                    "(\\body the theme-concrete-root-layout (root-layout layout) " +
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
        
        JExpression ss = new JExpression(
                _(GET._, "page",
                    _(AN._, "rest",
                        _(USE._, "favicon.ico"),
                        _(AN._, "uri", value("/favicon.ico")),
                        _(AN._, "host", value("localhost"))
                    )
                )
            );
        assertAnimoResult(ss, "");
    }

    @Test
    public void form_generator() throws Exception {
        __(
        		"the firstname (name \"firstname\").",
        		"the lastname (name \"lastname\").",
        		"the person (name \"person\") (part (firstname) (lastname)).",
        		
        		"the html-form \\form (@method \"POST\") (@action string (\"/\") (id this part) (\"/store\")) (\\p get name) (each (get part) (\\input (@name id this part) (get name))) (\\input @type \"submit\")."
		);
    	
        JExpression test = new JExpression(
            _(AN._, "html-form",
                _(AN._, "part",
            		_(AN._, "person")
        		)
            )
        );
        assertAnimoResult(test, "html-form \\form (@method \"POST\") (@action \"/person/store\") (\\p name \"person\") (\\input (@name \"person\") (name \"person\")) (\\input @type \"submit\").");

        assertXMLResult(test,
        		"<form><p>person</p><input name=\"person\">person</input><input type=\"submit\"/></form>");
    }
}
