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
package org.animotron.statement.operator;

import org.animotron.ATest;
import org.junit.Test;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class AnyTest extends ATest {
	
    @Test
    public void testANY() throws Throwable {

        __(
                "def B (^A) (v 'B').",
                "def C (^B) (v 'C')."
        );

        assertAnimoResultOneStep("any A", "B (^A) (v \"B\").");
//        assertAnimoResultOneStep(test, "C (B) (v \"C\").");

        assertAnimoResultOneStep("any B", "C (^B) (v \"C\").");
    }
	
    @Test
    public void testANYwithWITH() throws Throwable {

        __(
                "def B (^A) (v 'B').",
                "def B1 (^B) (v 'B1').",
                "def C (^B) (v 'C').",
                "def C1 (^C) (v 'C1')."
        );

        assertAnimoResultOneStep("any A with v 'B'", "B (^A) (v \"B\").");
//        assertAnimoResultOneStep(test, "B1 (B) (v \"B\").");

        assertAnimoResultOneStep("any A with v 'C'", "C (^B) (v \"C\").");
//        assertAnimoResultOneStep(test, "C1 (C) (v \"C\").");
    }

	@Test
	public void ANYwithEQ() throws Throwable {

        __(
                "def v-plain (mime-type) (v) (type 'v/plain') (reference 'Plain v') (extension 'txt').",
                "def application-atom (mime-type) (type 'application/atom+xml') (reference 'Atom Feed Document') (extension 'atom')."
        );

		assertAnimoResultOneStep("any mime-type eq (extension) ('txt')", "v-plain (mime-type) (v) (type \"v/plain\") (reference \"Plain v\") (extension \"txt\").");
	}

	@Test
	public void test_01() throws Throwable {
		__(
			"def x1 1.",
			"def x2 2.",
			"def y1 (z) (foo x1).",
			"def y2 (z) (foo x2)."
		);
		assertAnimoResult("any z with foo x2", "y2 (z) (foo).");
	}

	@Test
	public void test_02() throws Throwable {
		__(
			"def x1 (foo) 1.",
			"def x2 (foo) 2.",
			"def y1 (z) (^x1).",
			"def y2 (z) (^x2)."
		);
		assertAnimoResult("get foo y1", "x1 (foo) 1.");		
		assertAnimoResult("any z with foo x2", "y2 (z) (x2 (foo) 2).");
	}

    @Test
    public void test_03() throws Throwable {

        __(
                "def A (^S) (X 'α').",
                "def B (^A) (Y 'β').",
                "def C (^B) (Z 'γ')."
        );

        assertAnimoResultOneStep("any S with X 'α'", "A (^S) (X \"α\").");

        assertAnimoResultOneStep("any S with Y 'β'", "B (^A) (Y \"β\").");

        assertAnimoResultOneStep("any S with Z 'γ'", "C (^B) (Z \"γ\").");
    }

    @Test
    public void test_04() throws Throwable {
        __(
            "def foo-site (site) (server-name \"foo.com\").",
            "def bar-site (site) (server-name \"bar.com\").",

            "def hello-foo (^foo-site, root) (title \"hello foo\").",
            "def hello-bar (^bar-site, root) (title \"hello bar\").",
            
            "def foo-root-layout (layout, foo, root) (h1)."
		);

        assertAnimoResult(
    		"any root with server-name \"foo.com\"", 
    		"hello-foo (foo-site (site) (server-name)) (root) (title).");
    }

    @Test
    public void test_05() throws Throwable {

        __(
            "def foo-site (site) (server-name \"foo.com\") (weak-use foo).",
            "def bar-site (site) (server-name \"bar.com\") (weak-use bar).",

            "def hello-foo (foo-site, root) (title \"hello foo\").",
            "def hello-bar (bar-site, root) (title \"hello bar\").",
            
            "def foo-root-layout (layout, foo, root) (h1)."
        );

        assertAnimoResult("any xxx with server-name \"foo.com\"", "");
    }
}
