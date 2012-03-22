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
import org.junit.Test;

import java.io.IOException;

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
                _(ANY._, service,
                        _(WITH._, "server-name", value(site))
                )
        );
    }

    private Expression mime(Expression query) {
        return new JExpression(_(GET._, "type", _(GET._, "mime-type", _(query))));
    }

    private void assertQuery(String site, String service, String mime, String html) throws IOException, InterruptedException {
        Expression e = query(site, service);
        assertStringResult(mime(e), mime);
        assertHtmlResult(e, html);
    }

    @Test
    public void test() throws Throwable {

        __(
            "the foo-site (site) (server-name \"foo.com\") (weak-use foo)",
            "the bar-site (site) (server-name \"bar.com\") (weak-use bar)",

            "the text-html (mime-type) (type \"text/html\") (extension \"htm\" \"html\")",
            "the html-page (mime-tipe text-html) (\\html (\\head \\title get title) (\\body any layout))",

            "the hello-foo (html-page) (foo-site, root) (use root) (title \"hello foo\") (content \"foo foo foo\")",
            "the hello-bar (html-page) (bar-site, root) (use root) (title \"hello bar\") (content \"bar bar bar\")",
            
            "the xxx-service (html-page) (foo-site, bar-site, xxx) (use xxx) (title \"hello xxx\") (content \"xxx xxx xxx\")",

            "the zzz-service (html-page) (foo-site, zzz) (use qLayout) (title \"hello zzz\") (content \"zzz zzz zzz\")",
            "the yyy-service (html-page) (bar-site, yyy) (use qLayout) (title \"hello yyy\") (content \"yyy yyy yyy\")",

            "the foo-root-layout (layout, foo, root) (\\h1 get title) (\\p get content)",
            "the bar-root-layout (layout, bar, root) (\\h2 get title) (\\div get content)",
            
            "the foo-xxx-layout (layout, foo, xxx) (\\h3 get title) (\\p get content) (\\p get server-name)",
            "the bar-xxx-layout (layout, bar, xxx) (\\h4 get title) (\\div get content) (\\p get server-name)",

            "the qLayout (layout) (\\h3 get title) (\\span get content)"
        );

        assertQuery("foo.com", "root", "text/html",
                "<html><head><title>hello foo</title></head><body><h1>hello foo</h1><p>foo foo foo</p></body></html>");

        assertQuery("foo.com", "xxx", "text/html",
                "<html><head><title>hello xxx</title></head><body><h3>hello xxx</h3><p>xxx xxx xxx</p><p>foo.com</p></body></html>");

        assertQuery("foo.com", "yyy", "", "");

        assertQuery("foo.com", "zzz", "text/html",
                "<html><head><title>hello zzz</title></head><body><h3>hello zzz</h3><span>zzz zzz zzz</span></body></html>");

        assertQuery("bar.com", "root", "text/html",
                "<html><head><title>hello bar</title></head><body><h2>hello bar</h2><div>bar bar bar</div></body></html>");

        assertQuery("bar.com", "xxx", "text/html",
                "<html><head><title>hello xxx</title></head><body><h4>hello xxx</h4><div>xxx xxx xxx</div><p>bar.com</p></body></html>");

        assertQuery("bar.com", "yyy", "text/html",
                "<html><head><title>hello yyy</title></head><body><h3>hello yyy</h3><span>yyy yyy yyy</span></body></html>");

        assertQuery("bar.com", "zzz", "", "");

    }
}
