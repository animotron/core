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
                _(AN._,
                        _(GET._, service,
                                _(ANY._, "site",
                                        _(WITH._, "server-name", value(site))
                                )
                        )
                )
        );
    }

    private Expression error(String site, int code, String trace) {
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

    private Expression mime(Expression query) {
        return new JExpression(_(GET._, "type", _(GET._, "mime-type", _(query))));
    }

    private void assertQuery(String site, String service, String mime, String html) throws IOException, InterruptedException {
        Expression e = query(site, service);
        assertStringResult(mime(e), mime);
        assertHtmlResult(e, html);
    }

    private void assertError(String site, int code, String trace, String mime, String html) throws IOException, InterruptedException {
        Expression e = error(site, code, trace);
        assertStringResult(mime(e), mime);
        assertHtmlResult(e, html);
    }

    @Test
    public void test() throws Throwable {

        __(
                "def site (not-found-error default-not-found) (xxx xxx-service)",
                "def not-found-error (error) (code 404)"
        );

        assertAnimoResult("get not-found-error site", "not-found-error default-not-found.");
    }

    @Test
    public void test_00() throws Throwable {

        __(
                "def site (not-found-error default-not-found) (xxx xxx-service)",

                "def foo-site (site) (server-name \"foo.com\") (weak-use foo) (root hello-foo) (zzz zzz-service)",
                "def bar-site (site) (server-name \"bar.com\") (weak-use bar) (root hello-bar) (yyy yyy-service) (not-found-error bar-not-found)",

                "def text-html (#mime-type) (type \"text/html\") (extension \"htm\" \"html\")",
                "def html-page (text-html) (\\html (\\head \\title get title) (\\body any layout))",

                "def hello-foo (html-page) (use root) (title \"hello foo\") (content \"foo foo foo\")",
                "def hello-bar (html-page) (use root) (title \"hello bar\") (content \"bar bar bar\")",

                "def xxx-service (html-page) (use xxx) (title \"hello xxx\") (content \"xxx xxx xxx\")",

                "def zzz-service (html-page) (use qLayout) (title \"hello zzz\") (content \"zzz zzz zzz\")",
                "def yyy-service (html-page) (use qLayout) (title \"hello yyy\") (content \"yyy yyy yyy\")",

                "def foo-root-layout (layout, foo, root) (\\h1 get title) (\\p get content)",
                "def bar-root-layout (layout, bar, root) (\\h2 get title) (\\div get content)",

                "def foo-xxx-layout (layout, foo, xxx) (\\h3 get title) (\\p get content) (\\p get server-name)",
                "def bar-xxx-layout (layout, bar, xxx) (\\h4 get title) (\\div get content) (\\p get server-name)",

                "def qLayout (layout) (\\h3 get title) (\\span get content)",

                "def not-found-error (error) (code 404)",

                "def default-not-found (html-page) (use default-error-layout) (title \"Not found\") (message \"Not found anything\")",

                "def bar-not-found (html-page) (use error) (title \"Error. Not found\") (message \"Sorry, not found anything\")",

                "def default-error-layout (layout) (\\h1 get code) (\\h2 get title) (\\p get message) (\\p get stack-trace)",
                "def bar-errogr-layout (layout, bar, error) (\\h1 get code) (\\h2 get title) (\\div get message) (\\div get stack-trace)"

        );

        assertAnimoResult("any error with code 404", "def not-found-error (error) (code).");
        assertAnimoResult("get not-found-error site", "not-found-error default-not-found (html-page (text-html (mime-type) (type) (extension)) (\\html (\\head \\title title \"Not found\") (\\body def default-error-layout (layout) (\\h1) (\\h2 title \"Not found\") (\\p message \"Not found anything\") (\\p)))) (use default-error-layout) (title) (message).");
        assertAnimoResult("get (any error with code 404) (site)", "not-found-error default-not-found (html-page (text-html (mime-type) (type) (extension)) (\\html (\\head \\title title \"Not found\") (\\body def default-error-layout (layout) (\\h1) (\\h2 title \"Not found\") (\\p message \"Not found anything\") (\\p)))) (use default-error-layout) (title) (message).");

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

        __(
                "def site",
                "def not-found-error",
                "def default-not-found",
                "def xxx",
                "def xxx-service",
                "def foo-site",
                "def site",
                "def server-name",
                "def foo",
                "def root",
                "def hello-foo",
                "def zzz",
                "def zzz-service",
                "def bar-site",
                "def bar",
                "def hello-bar",
                "def yyy",
                "def yyy-service",
                "def bar-not-found",
                "def text-html",
                "def mime-type",
                "def type",
                "def extension",
                "def html-page",
                "def title",
                "def layout",
                "def content",
                "def qLayout",
                "def foo-root-layout",
                "def bar-root-layout",
                "def foo-xxx-layout",
                "def bar-xxx-layout",
                "def code",
                "def error",
                "def message",
                "def stack-trace",

                "def site (not-found-error default-not-found) (xxx xxx-service)",

                "def foo-site (site) (server-name \"foo.com\") (weak-use foo) (root hello-foo) (zzz zzz-service)",
                "def bar-site (site) (server-name \"bar.com\") (weak-use bar) (root hello-bar) (yyy yyy-service) (not-found-error bar-not-found)",

                "def text-html (mime-type) (type \"text/html\") (extension \"htm\" \"html\")",
                "def html-page (mime-type text-html) (\\html (\\head \\title get title) (\\body any layout))",

                "def hello-foo (html-page) (use root) (title \"hello foo\") (content \"foo foo foo\")",
                "def hello-bar (html-page) (use root) (title \"hello bar\") (content \"bar bar bar\")",

                "def xxx-service (html-page) (use xxx) (title \"hello xxx\") (content \"xxx xxx xxx\")",

                "def zzz-service (html-page) (use qLayout) (title \"hello zzz\") (content \"zzz zzz zzz\")",
                "def yyy-service (html-page) (use qLayout) (title \"hello yyy\") (content \"yyy yyy yyy\")",

                "def foo-root-layout (layout, foo, root) (\\h1 get title) (\\p get content)",
                "def bar-root-layout (layout, bar, root) (\\h2 get title) (\\div get content)",

                "def foo-xxx-layout (layout, foo, xxx) (\\h3 get title) (\\p get content) (\\p get server-name)",
                "def bar-xxx-layout (layout, bar, xxx) (\\h4 get title) (\\div get content) (\\p get server-name)",

                "def qLayout (layout) (\\h3 get title) (\\span get content)",

                "def not-found-error (error) (code 404)",

                "def default-not-found (html-page) (use default-error-layout) (title \"Not found\") (message \"Not found anything\")",

                "def bar-not-found (html-page) (use error) (title \"Error. Not found\") (message \"Sorry, not found anything\")",

                "def default-error-layout (layout) (\\h1 get code) (\\h2 get title) (\\p get message) (\\p get stack-trace)",
                "def bar-error-layout (layout, bar, error) (\\h1 get code) (\\h2 get title) (\\div get message) (\\div get stack-trace)"

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

        __(
                "def site (not-found-error default-not-found) (xxx xxx-service)",

                "def foo-site (site) (server-name \"foo.com\") (weak-use foo) (root hello-foo) (zzz zzz-service)",
                "def bar-site (site) (server-name \"bar.com\") (weak-use bar) (root hello-bar) (yyy yyy-service) (not-found-error bar-not-found)",

                "def text-html (mime-type) (type \"text/html\") (extension \"htm\" \"html\")",
                "def html-page (mime-type text-html) (\\html (\\head \\title get title) (\\body any layout))",

                "def hello-foo (html-page) (use root) (title \"hello foo\") (content \"foo foo foo\")",
                "def hello-bar (html-page) (use root) (title \"hello bar\") (content \"bar bar bar\")",

                "def xxx-service (html-page) (use xxx) (title \"hello xxx\") (content \"xxx xxx xxx\")",

                "def zzz-service (html-page) (use qLayout) (title \"hello zzz\") (content \"zzz zzz zzz\")",
                "def yyy-service (html-page) (use qLayout) (title \"hello yyy\") (content \"yyy yyy yyy\")",

                "def foo-root-layout (layout, foo, root) (\\h1 get title) (\\p get content)",
                "def bar-root-layout (layout, bar, root) (\\h2 get title) (\\div get content)",

                "def foo-xxx-layout (layout, foo, xxx) (\\h3 get title) (\\p get content) (\\p get server-name)",
                "def bar-xxx-layout (layout, bar, xxx) (\\h4 get title) (\\div get content) (\\p get server-name)",

                "def qLayout (layout) (\\h3 get title) (\\span get content)",

                "def not-found-error (error) (code 404)",

                "def default-not-found (html-page) (use default-error-layout) (title \"Not found\") (message \"Not found anything\")",

                "def bar-not-found (html-page) (use error) (title \"Error. Not found\") (message \"Sorry, not found anything\")",

                "def default-error-layout (layout) (\\h1 get code) (\\h2 get title) (\\p get message) (\\p get stack-trace)",
                "def bar-error-layout (layout, bar, error) (\\h1 get code) (\\h2 get title) (\\div get message) (\\div get stack-trace)"

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
    public void test_03() throws Throwable {

        __(
                "def foo-site (site) (server-name \"foo.com\") (weak-use foo) (root hello-foo) (xxx xxx-service) (zzz zzz-service)",
                "def bar-site (site) (server-name \"bar.com\") (weak-use bar) (root hello-bar) (xxx xxx-service) (yyy yyy-service)",

                "def text-html (mime-type) (type \"text/html\") (extension \"htm\" \"html\")",
                "def html-page (mime-type text-html) (\\html (\\head \\title get title) (\\body any layout))",

                "def hello-foo (html-page) (use root) (title \"hello foo\") (content \"foo foo foo\")",
                "def hello-bar (html-page) (use root) (title \"hello bar\") (content \"bar bar bar\")",

                "def xxx-service (html-page) (use xxx) (title \"hello xxx\") (content \"xxx xxx xxx\")",

                "def zzz-service (html-page) (use qLayout) (title \"hello zzz\") (content \"zzz zzz zzz\")",
                "def yyy-service (html-page) (use qLayout) (title \"hello yyy\") (content \"yyy yyy yyy\")",

                "def foo-root-layout (layout, foo, root) (\\h1 get title) (\\p get content)",
                "def bar-root-layout (layout, bar, root) (\\h2 get title) (\\div get content)",

                "def foo-xxx-layout (layout, foo, xxx) (\\h3 get title) (\\p get content) (\\p get server-name)",
                "def bar-xxx-layout (layout, bar, xxx) (\\h4 get title) (\\div get content) (\\p get server-name)",

                "def qLayout (layout) (\\h3 get title) (\\span get content)"
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
    @Test
    public void test_04() throws Throwable {

        __(
                "def site (not-found-error default-not-found) (xxx xxx-service)",

                "def foo-site (site) (server-name \"foo.com\") (weak-use foo) (root hello-foo)",

                "def text-html (#mime-type) (type \"text/html\") (extension \"htm\" \"html\")",
                "def html-page (text-html) (\\html (\\head \\title get title) (\\body any layout))",

                "def hello-foo (html-page) (use root) (title \"hello foo\") (content \"foo foo foo\")",

                "def foo-root-layout (layout, foo, root) (\\h1 get title) (\\p get content)",

                "def not-found-error (error) (code 404)",

                "def default-not-found (html-page) (use default-error-layout) (title \"Not found\") (message \"Not found anything\")",

                "def default-error-layout (layout) (\\h1 get code) (\\h2 get title) (\\p get message) (\\p get stack-trace)",
                "def bar-error-layout (layout, bar, error) (\\h1 get code) (\\h2 get title) (\\div get message) (\\div get stack-trace)"

        );

        assertError("foo.com", 404, "stack trace would be here", "text/html",
                "<html><head><title>Not found</title></head><body><h1>404</h1><h2>Not found</h2><p>Not found anything</p><p>stack trace would be here</p></body></html>"
        );

        assertQuery("foo.com", "root", "text/html",
                "<html><head><title>hello foo</title></head><body><h1>hello foo</h1><p>foo foo foo</p></body></html>");

    }

    @Test
    public void test_05() throws Throwable {

        __(
                "def site (not-found-error default-not-found) (xxx xxx-service)",

                "def foo-site (site) (server-name \"foo.com\") (weak-use foo) (root hello-foo)",

                "def text-html (mime-type) (type \"text/html\") (extension \"htm\" \"html\")",
                "def html-page (mime-type text-html) (\\html (\\head \\title get title) (\\body any layout))",

                "def hello-foo (html-page) (use root) (title \"hello foo\") (content \"foo foo foo\")",

                "def foo-root-layout (layout, foo, root) (\\h1 get title) (\\p get content)",

                "def not-found-error (error) (code 404)",

                "def default-not-found (html-page) (use default-error-layout) (title \"Not found\") (message \"Not found anything\")",

                "def default-error-layout (layout) (\\h1 get code) (\\h2 get title) (\\p get message) (\\p get stack-trace)",
                "def bar-error-layout (layout, bar, error) (\\h1 get code) (\\h2 get title) (\\div get message) (\\div get stack-trace)"

        );

        assertError("foo.com", 404, "stack trace would be here", "text/html",
                "<html><head><title>Not found</title></head><body><h1>404</h1><h2>Not found</h2><p>Not found anything</p><p>stack trace would be here</p></body></html>"
        );

        assertQuery("foo.com", "root", "text/html",
                "<html><head><title>hello foo</title></head><body><h1>hello foo</h1><p>foo foo foo</p></body></html>");

    }

}
