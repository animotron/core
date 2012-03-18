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
import org.animotron.expression.AnimoExpression;
import org.animotron.expression.Expression;
import org.animotron.expression.JExpression;
import org.animotron.statement.query.GET;
import org.junit.Test;

import static org.animotron.expression.AnimoExpression.__;
import static org.animotron.expression.JExpression._;


/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class CurrentWebFrameworkTest extends ATest {

    @Test
    public void test() throws Throwable {

        __(
            "the site any service",
            
            "the text-html (mime-type) (type \"text/html\") (extrnsion \"htm\" \"html\")",
            "the html-page (mime-tipe text-html) (\\html (\\head \\title get title) (\\body any layout))",
            
            "the hello-foo (html-page) (service) (root) (foo) (title \"hello foo\") (content \"foo foo foo\")",
            "the hello-bar (html-page) (service) (root) (bar) (title \"hello bar\") (content \"bar bar bar\")",
            
            "the xxx (html-page, service) (title \"hello world\") (content \"xxx xxx xxx\")",
            "the yyy (html-page, service) (use xxx-layout) (title \"hello hell\") (content \"yyy yyy yyy\")",
            //"the xxx-bar (xxx) (bar).",
            //"the xxx-layout-bar (xxx-layout) (bar).",
            
            "the foo-root-layout (layout, foo, root) (\\h1 get title) (\\p get content)",
            "the bar-root-layout (layout, bar, root) (\\h2 get title) (\\div get content)",
            
            "the xxx-layout (layout, xxx) (\\h3 get title) (\\span get content)",
            
            "the foo-site (site) (server-name \"foo.com\") (weak-use foo)",
            
            "the bar-site (site) (server-name \"bar.com\") (weak-use bar) (bar (xxx) (yyy))"
        );

        Expression q1 = new AnimoExpression("any site (with server-name \"foo.com\") (use root)");
        Expression q2 = new AnimoExpression("any site (with server-name \"foo.com\") (use xxx)");
        Expression q3 = new AnimoExpression("any site (with server-name \"foo.com\") (use yyy)");

        Expression q4 = new AnimoExpression("any site (with server-name \"bar.com\") (use root)");
        Expression q5 = new AnimoExpression("any site (with server-name \"bar.com\") (use xxx)");
        Expression q6 = new AnimoExpression("any site (with server-name \"bar.com\") (use yyy)");

        Expression m1 = new JExpression(_(GET._, "type", _(GET._, "mime-type", _(q1))));
        Expression m2 = new JExpression(_(GET._, "type", _(GET._, "mime-type", _(q2))));
        Expression m3 = new JExpression(_(GET._, "type", _(GET._, "mime-type", _(q3))));
        Expression m4 = new JExpression(_(GET._, "type", _(GET._, "mime-type", _(q4))));
        Expression m5 = new JExpression(_(GET._, "type", _(GET._, "mime-type", _(q5))));
        Expression m6 = new JExpression(_(GET._, "type", _(GET._, "mime-type", _(q6))));

        assertStringResult(m1, "text/html");
        assertStringResult(m2, "");
        assertStringResult(m3, "");
        assertStringResult(m4, "text/html");
        assertStringResult(m5, "text/html");
        assertStringResult(m6, "text/html");

        assertHtmlResult(q1,
    		"<html><head><title>hello foo</title></head><body><h1>hello foo</h1><p>foo foo foo</p></body></html>");

        assertHtmlResult(q2, "");

        assertHtmlResult(q3, "");

        assertHtmlResult(q4,
    		"<html><head><title>hello bar</title></head><body><h2>hello bar</h2><div>bar bar bar</div></body></html>");

        assertHtmlResult(q5,
    		"<html><head><title>hello world</title></head><body/></html>");

        assertHtmlResult(q6,
    		"<html><head><title>hello world</title></head><body/></html>");
    }
}