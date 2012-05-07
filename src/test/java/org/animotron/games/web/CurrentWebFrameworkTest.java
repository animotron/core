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
import org.animotron.statement.operator.AN;
import org.animotron.statement.query.ANY;
import org.animotron.statement.query.GET;
import org.junit.Test;

import static org.animotron.expression.JExpression._;
import static org.animotron.expression.JExpression.value;


/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class CurrentWebFrameworkTest extends ATest {

    private Expression query(String site, String service) throws Throwable {
        return new JExpression(
                _(AN._,
                        _(GET._, service,
                                _(ANY._, "site",
                                        _(WITH._, "server-name", value(site))
                                )
                        )
                )
        );
    }

    private Expression error(String site, int code, String trace) throws Throwable {
        return new JExpression(
                _(AN._,
                        _(GET._,
                                _(ANY._, "error",
                                        _(WITH._, "code", value(code))
                                ),
                                _(ANY._, "site",
                                        _(WITH._, "server-name", value(site))
                                )
                        ),
                        _(AN._, "stack-trace", value(trace))
                )
        );
    }

    private Expression mime(Expression query) throws Throwable {
        return new JExpression(_(GET._, "type", _(GET._, "mime-type", _(query))));
    }

    private void assertQuery(String site, String service, String mime, String html) throws Throwable, InterruptedException {
        Expression e = query(site, service);
        assertStringResult(mime(e), mime);
        assertHtmlResult(e, html);
    }

    private void assertError(String site, int code, String trace, String mime, String html) throws Throwable, InterruptedException {
        Expression e = error(site, code, trace);
        assertStringResult(mime(e), mime);
        assertHtmlResult(e, html);
    }

    @Test
    public void test_00() throws Throwable {

        testAnimo(
                "the site",
                "the not-found-error",
                "the default-not-found",
                "the xxx",
                "the xxx-service",
                "the foo-site",
                "the site",
                "the server-name",
                "the foo",
                "the root",
                "the hello-foo",
                "the zzz",
                "the zzz-service",
                "the bar-site",
                "the bar",
                "the hello-bar",
                "the yyy",
                "the yyy-service",
                "the bar-not-found",
                "the text-html",
                "the mime-type",
                "the type",
                "the extension",
                "the html-page",
                "the title",
                "the layout",
                "the content",
                "the qLayout",
                "the foo-root-layout",
                "the bar-root-layout",
                "the foo-xxx-layout",
                "the bar-xxx-layout",
                "the code",
                "the error",
                "the message",
                "the stack-trace",

                "the site (not-found-error default-not-found) (xxx xxx-service)",

                "the foo-site (site) (server-name \"foo.com\") (weak-use foo) (root hello-foo) (zzz zzz-service)",
                "the bar-site (site) (server-name \"bar.com\") (weak-use bar) (root hello-bar) (yyy yyy-service) (not-found-error bar-not-found)",

                "the text-html (mime-type) (type \"text/html\") (extension \"htm\" \"html\")",
                "the html-page (mime-type text-html) (\\html (\\head \\title get title) (\\body any layout))",

                "the hello-foo (html-page) (use root) (title \"hello foo\") (content \"foo foo foo\")",
                "the hello-bar (html-page) (use root) (title \"hello bar\") (content \"bar bar bar\")",

                "the xxx-service (html-page) (use xxx) (title \"hello xxx\") (content \"xxx xxx xxx\")",

                "the zzz-service (html-page) (use qLayout) (title \"hello zzz\") (content \"zzz zzz zzz\")",
                "the yyy-service (html-page) (use qLayout) (title \"hello yyy\") (content \"yyy yyy yyy\")",

                "the foo-root-layout (layout, foo, root) (\\h1 get title) (\\p get content)",
                "the bar-root-layout (layout, bar, root) (\\h2 get title) (\\div get content)",

                "the foo-xxx-layout (layout, foo, xxx) (\\h3 get title) (\\p get content) (\\p get server-name)",
                "the bar-xxx-layout (layout, bar, xxx) (\\h4 get title) (\\div get content) (\\p get server-name)",

                "the qLayout (layout) (\\h3 get title) (\\span get content)",

                "the not-found-error (error) (code 404)",

                "the default-not-found (html-page) (use default-error-layout) (title \"Not found\") (message \"Not found anything\")",

                "the bar-not-found (html-page) (use error) (title \"Error. Not found\") (message \"Sorry, not found anything\")",

                "the default-error-layout (layout) (\\h1 get code) (\\h2 get title) (\\p get message) (\\p get stack-trace)",
                "the bar-error-layout (layout, bar, error) (\\h1 get code) (\\h2 get title) (\\div get message) (\\div get stack-trace)"

        );

        assertError("foo.com", 404, "stack trace would be here", "text/html",
                "<html><head><title>Not found</title></head><body><h1>404</h1><h2>Not found</h2><p>Not found anything</p><p>stack trace would be here</p></body></html>"
        );

        assertError("foo.com", 500, "", "", "");

        assertError("bar.com", 404, "stack trace", "text/html",
                "<html><head><title>Error. Not found</title></head><body><h1>404</h1><h2>Error. Not found</h2><div>Sorry, not found anything</div><div>stack trace</div></body></html>"
        );

        assertError("bar.com", 500, "", "", "");

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

    @Test
    public void test_01() throws Throwable {

        testAnimo(
                "the site (not-found-error default-not-found) (xxx xxx-service)",

                "the foo-site (site) (server-name \"foo.com\") (weak-use foo) (root hello-foo) (zzz zzz-service)",
                "the bar-site (site) (server-name \"bar.com\") (weak-use bar) (root hello-bar) (yyy yyy-service) (not-found-error bar-not-found)",

                "the text-html (mime-type) (type \"text/html\") (extension \"htm\" \"html\")",
                "the html-page (mime-type text-html) (\\html (\\head \\title get title) (\\body any layout))",

                "the hello-foo (html-page) (use root) (title \"hello foo\") (content \"foo foo foo\")",
                "the hello-bar (html-page) (use root) (title \"hello bar\") (content \"bar bar bar\")",

                "the xxx-service (html-page) (use xxx) (title \"hello xxx\") (content \"xxx xxx xxx\")",

                "the zzz-service (html-page) (use qLayout) (title \"hello zzz\") (content \"zzz zzz zzz\")",
                "the yyy-service (html-page) (use qLayout) (title \"hello yyy\") (content \"yyy yyy yyy\")",

                "the foo-root-layout (layout, foo, root) (\\h1 get title) (\\p get content)",
                "the bar-root-layout (layout, bar, root) (\\h2 get title) (\\div get content)",

                "the foo-xxx-layout (layout, foo, xxx) (\\h3 get title) (\\p get content) (\\p get server-name)",
                "the bar-xxx-layout (layout, bar, xxx) (\\h4 get title) (\\div get content) (\\p get server-name)",

                "the qLayout (layout) (\\h3 get title) (\\span get content)",

                "the not-found-error (error) (code 404)",

                "the default-not-found (html-page) (use default-error-layout) (title \"Not found\") (message \"Not found anything\")",

                "the bar-not-found (html-page) (use error) (title \"Error. Not found\") (message \"Sorry, not found anything\")",

                "the default-error-layout (layout) (\\h1 get code) (\\h2 get title) (\\p get message) (\\p get stack-trace)",
                "the bar-error-layout (layout, bar, error) (\\h1 get code) (\\h2 get title) (\\div get message) (\\div get stack-trace)"

        );

        assertError("foo.com", 404, "stack trace would be here", "text/html",
                "<html><head><title>Not found</title></head><body><h1>404</h1><h2>Not found</h2><p>Not found anything</p><p>stack trace would be here</p></body></html>"
        );

        assertError("foo.com", 500, "", "", "");

        assertError("bar.com", 404, "stack trace", "text/html",
                "<html><head><title>Error. Not found</title></head><body><h1>404</h1><h2>Error. Not found</h2><div>Sorry, not found anything</div><div>stack trace</div></body></html>"
        );

        assertError("bar.com", 500, "", "", "");

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

    @Test
    public void test_02() throws Throwable {

        testAnimo(
                "the foo-site (site) (server-name \"foo.com\") (weak-use foo) (root hello-foo) (xxx xxx-service) (zzz zzz-service)",
                "the bar-site (site) (server-name \"bar.com\") (weak-use bar) (root hello-bar) (xxx xxx-service) (yyy yyy-service)",

                "the text-html (mime-type) (type \"text/html\") (extension \"htm\" \"html\")",
                "the html-page (mime-type text-html) (\\html (\\head \\title get title) (\\body any layout))",

                "the hello-foo (html-page) (use root) (title \"hello foo\") (content \"foo foo foo\")",
                "the hello-bar (html-page) (use root) (title \"hello bar\") (content \"bar bar bar\")",

                "the xxx-service (html-page) (use xxx) (title \"hello xxx\") (content \"xxx xxx xxx\")",

                "the zzz-service (html-page) (use qLayout) (title \"hello zzz\") (content \"zzz zzz zzz\")",
                "the yyy-service (html-page) (use qLayout) (title \"hello yyy\") (content \"yyy yyy yyy\")",

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
