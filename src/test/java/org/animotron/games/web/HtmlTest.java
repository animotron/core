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
import org.junit.Test;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class HtmlTest extends ATest {

    @Test
    public void test_00() throws Throwable {
        tAnimo("def el '<' (id this el) '>' (get 2) '</' (id this el) '>'.");
        tAnimo("def p el.");
        assertStringResult("p 'para'", "<p>para</p>");
    }

    @Test
    public void test_01() throws Throwable {
        tAnimo("def el '<' (id this el) '>' (get 2) '</' (id this el) '>'.");
        tAnimo("def ul el.");
        tAnimo("def li el.");
        assertStringResult("ul 1", "<ul>1</ul>");
        assertStringResult("ul li", "<ul><li></li></ul>");
        assertStringResult("ul li 1", "<ul><li>1</li></ul>");
    }

    @Test
    public void test_03() throws Throwable {
        tAnimo("def ul '<ul>' (get 1) '</ul>'.");
        tAnimo("def li '<li>' (get 1) '</li>'.");
        assertStringResult("ul li 1", "<ul><li>1</li></ul>");
    }

    @Test
    public void test_04() throws Throwable {
        tAnimo("def el '<' (id this el) '>' (get c) '</' (id this el) '>'.");
        tAnimo("def ul el.");
        tAnimo("def li el.");
        assertStringResult("ul c li c 1", "<ul><li>1</li></ul>");
    }

    @Test
    public void test_05() throws Throwable {
        tAnimo("def el '<' (id this el) '>' (get 2) '</' (id this el) '>'.");
        tAnimo("def html el.");
        tAnimo("def head el.");
        tAnimo("def title el.");
        tAnimo("def script '<script src=\\'' (get 1) '\\'></script>'.");
        tAnimo("def inline-script el.");
        tAnimo("def body el.");
        tAnimo("def h1 el.");
        tAnimo("def p el.");
        tAnimo("def page html (head title get caption) (body (h1 get title) (p get para) (script 'jquery.js') (inline-script 'alert(\\'' (get title) '\\');')).");
        assertStringResult("page caption 'Hello world!'", "<html><head><title>Hello world!</title></head><body><h1>Hello world!</h1><p></p><script src=\"jquery.js\"></script><inline-script>alert(\"Hello world!\");</inline-script></body></html>");
    }

    @Test
    public void test_06() throws Throwable {
        tAnimo("def el '<' (id this el) '>' (get 2) '</' (id this el) '>'.");
        tAnimo("def html el.");
        tAnimo("def head el.");
        tAnimo("def title el.");
        tAnimo("def script '<script src=\\'' (get 1) '\\'></script>'.");
        tAnimo("def inline-script el.");
        tAnimo("def body el.");
        tAnimo("def h1 el.");
        tAnimo("def p el.");
        tAnimo("def page html (head title get caption) (body (h1 get title) (p get para) (script 'jquery.js') (inline-script 'alert(\\'' (get title) '\\');')).");
        tAnimo("def hello (page) (caption 'Hello world!').");
        assertStringResult("hello", "<html><head><title>Hello world!</title></head><body><h1>Hello world!</h1><p></p><script src=\"jquery.js\"></script><inline-script>alert(\"Hello world!\");</inline-script></body></html>");
    }

    @Test
    public void test_07() throws Throwable {
        tAnimo("def el '<' (get name this el) '>' (get 2) '</' (get name this el) '>'.");
        tAnimo("def html (el) (name 'html').");
        tAnimo("def head (el) (name 'head').");
        tAnimo("def title (el) (name 'title').");
        tAnimo("def script '<script src=\\'' (get 1) '\\'></script>'.");
        tAnimo("def inline-script (el) (name 'script').");
        tAnimo("def body (el) (name 'body').");
        tAnimo("def h1 (el) (name 'h1').");
        tAnimo("def p (el) (name 'p').");
        tAnimo("def page html (head title get caption) (body (h1 get title) (p get para) (script 'jquery.js') (inline-script 'alert(\\'' (get title) '\\');')).");
        tAnimo("def hello (page) (caption 'Hello world!').");
        assertStringResult("hello", "<html><head><title>Hello world!</title></head><body><h1>Hello world!</h1><p></p><script src=\"jquery.js\"></script><script>alert(\"Hello world!\");</script></body></html>");
    }

    @Test
    public void test_08() throws Throwable {
        tAnimo("def meta '<meta' (each (get 1) (' ' (id this 1) '=\\\"' (get (this 1) (this)) '\\\"')) '>'.");
        assertStringResult("meta (a 1) (b 2) (c 3)", "<meta a=\"1\" b=\"2\" c=\"3\">");
    }

    @Test
    public void test_09() throws Throwable {
        tAnimo("def \\ '<' (id this) (each (get 1) (ptrn (this) (?is @ (' ' (id get @ this 1) '=\\\"' (get (get @ this 1) (get @ this 1)) '\\\"')))) '>' (each (get 1) (this)) '</' (id this) '>'.");
        assertStringResult("\\ p (@ class 'abc') (\\ strong 'foo') (\\ span (@ style 'display:none') 'bar' \\ a (@ href '#') 'home')", "<p class=\"abc\"><strong>foo</strong><span style=\"display:none\">bar<a href=\"#\">home</a></span></p>");
    }

    @Test
    public void test_10() throws Throwable {
        tAnimo("def \\ (this) (get @ this) (each (get 1) (this)).");
        assertAnimoResult("\\ a (@ b) (\\ c)", "\\ (a) (b) (@) (\\ c).");
    }

}
