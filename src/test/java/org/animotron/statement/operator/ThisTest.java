/*
 *  Copyright (C) 2011-2013 The Animo Project
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
package org.animotron.statement.operator;

import org.animotron.ATest;
import org.junit.Ignore;
import org.junit.Test;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class ThisTest extends ATest {

    @Test
    @Ignore //empty this
    public void test_00() throws Throwable {
        testAnimo("def file uri id this.");
        assertAnimoResult("get uri file.", "uri \"file\".");
    }

    @Test
    @Ignore //empty this
    public void test_01() throws Throwable {
        testAnimo("def file uri id this.");
        testAnimo("def x file.");
        assertAnimoResult("get uri x.", "uri \"x\".");
    }

    @Test
    @Ignore //empty this
    public void test_02() throws Throwable {
        testAnimo("def file uri id this.");
        testAnimo("def x file.");
        testAnimo("def y file.");
        assertAnimoResult("get uri x.", "uri \"x\".");
        assertAnimoResult("get uri y.", "uri \"y\".");
    }

    @Test
    public void test_10() throws Throwable {
        __(
    		"def p get 1.",
    		"def rainbow each (this) (p id this)."
		);
        assertAnimoResult("rainbow (red) (green).", "rainbow (p \"red\") (p \"green\").");
    }

    @Test
    public void test_11() throws Throwable {
        __(
    		"def app-layout each (get js this app) ('<js uri=' (get uri this) '/>').",
    		"def app app-layout.",
    		"def some-js uri \"some-js-uri\".",
    		"def IDE (app) (js some-js)."
		);
        assertAnimoResult("IDE", "IDE (app app-layout \\js \"some-js-uri\") (js).");
    }

    @Test
    public void test_12() throws Throwable {
        __(
    		"def app (html-page) (js foo).",
    		"def IDE (app) (js bar).",
    		"def html-page any layout.",
    		"def app-layout (layout) (get js this app)."
		);
        assertAnimoResult("IDE", "IDE (app (html-page app-layout (layout) (bar)) (js)) (js).");
    }
}
