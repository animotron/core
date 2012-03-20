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
import org.animotron.expression.Expression;
import org.animotron.expression.JExpression;
import org.animotron.statement.compare.WITH;
import org.animotron.statement.query.ANY;
import org.animotron.statement.query.GET;
import org.animotron.statement.relation.USE;
import org.junit.Test;

import static org.animotron.expression.AnimoExpression.__;
import static org.animotron.expression.JExpression._;
import static org.animotron.expression.JExpression.value;


/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class CurrentWebFrameworkTest extends ATest {

    private Expression query(String site, String service) {
        return new JExpression(
                _(ANY._, "site",
                        _(WITH._, "server-name", value(site)),
                        _(USE._, service)
                )
        );
    }

    private Expression mime(Expression query) {
        return new JExpression(_(GET._, "type", _(GET._, "mime-type", _(query))));
    }

    @Test
    public void test() throws Throwable {

        __(
            "the site any service",
            
            "the text-html (mime-type) (type \"text/html\") (extension \"htm\" \"html\")",
            "the html-page (mime-tipe text-html) (\\html (\\head \\title get title) (\\body any layout))",
            
            "the hello-foo (html-page) (service, root, foo) (title \"hello foo\") (content \"foo foo foo\")",
            "the hello-bar (html-page) (service, root, bar) (title \"hello bar\") (content \"bar bar bar\")",
            
            "the zzz-service (html-page) (service, zzz) (title \"hello world\") (content \"xxx xxx xxx\")",
            "the yyy-service (html-page) (service, yyy) (title \"hello hell\") (content \"yyy yyy yyy\")",

            "the foo-root-layout (layout, foo, root) (\\h1 get title) (\\p get content)",
            "the bar-root-layout (layout, bar, root) (\\h2 get title) (\\div get content)",
            
            "the qLayout (layout, zzz, yyy) (\\h3 get title) (\\span get content)",
            
            "the foo-site (site) (server-name \"foo.com\") (weak-use foo)",
            
            "the bar-site (site) (server-name \"bar.com\") (weak-use bar) (bar (yyy-service) (qLayout))."
        );

        //root service
        Expression fooRoot = query("foo.com", "root");
        //this service wasn't defined, so root should be returned? 
        Expression fooXxx = query("foo.com", "xxx");
        //this service defined, but do not allowed by site 
        Expression fooYyy = query("foo.com", "yyy");

        Expression barRoot = query("bar.com", "root");
        Expression barZzz = query("bar.com", "zzz");
        Expression barYyy = query("bar.com", "yyy");

        assertStringResult(mime(fooRoot), "text/html");
        assertStringResult(mime(fooXxx), "text/html");
        assertStringResult(mime(fooYyy), "");
        assertStringResult(mime(barRoot), "text/html");
        assertStringResult(mime(barZzz), "text/html");
        assertStringResult(mime(barYyy), "text/html");

        assertHtmlResult(fooRoot,
    		"<html><head><title>hello foo</title></head><body><h1>hello foo</h1><p>foo foo foo</p></body></html>");

        assertHtmlResult(fooXxx,
    		"<html><head><title>hello foo</title></head><body><h1>hello foo</h1><p>foo foo foo</p></body></html>");

        assertHtmlResult(fooYyy, "");

        assertHtmlResult(barRoot,
    		"<html><head><title>hello bar</title></head><body><h2>hello bar</h2><div>bar bar bar</div></body></html>");

        assertHtmlResult(barZzz,
    		"");

        assertHtmlResult(barYyy,
    		"<html><head><title>hello world</title></head><body/></html>");
    }
}
