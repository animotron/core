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
import org.animotron.expression.JExpression;
import org.animotron.statement.compare.EQ;
import org.animotron.statement.compare.WITH;
import org.animotron.statement.query.ANY;
import org.junit.Test;

import static org.animotron.expression.AnimoExpression.__;
import static org.animotron.expression.JExpression._;
import static org.animotron.expression.JExpression.value;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class AnyTest extends ATest {
	
    @Test
    public void testANY() throws Throwable {
        
        __(
            new JExpression(
                    _(DEF._, "A", _(AN._, "value"))
            ),
            new JExpression(
                    _(DEF._, "B", _(AN._, "A"), _(AN._, "value", value("B")))
            ),
            new JExpression(
                    _(DEF._, "C", _(AN._, "B"), _(AN._, "value", value("C")))
            )
        );

        JExpression test = new JExpression(
            _(ANY._, "A")
        );
        assertAnimoResultOneStep(test, "def B (A) (value \"B\").");
//        assertAnimoResultOneStep(test, "def C (B) (value \"C\").");

        test = new JExpression(
            _(ANY._, "B")
        );
        assertAnimoResultOneStep(test, "def C (B) (value \"C\").");
    }
	
    @Test
    public void testANYwithWITH() throws Throwable {

        __(
            new JExpression(
                    _(DEF._, "A", _(AN._, "value"))
            ),
            new JExpression(
                    _(DEF._, "B", _(AN._, "A"), _(AN._, "value", value("B")))
            ),
            new JExpression(
                    _(DEF._, "B1", _(AN._, "B"), _(AN._, "value", value("B1")))
            ),
            new JExpression(
                    _(DEF._, "C", _(AN._, "B"), _(AN._, "value", value("C")))
            ),
            new JExpression(
                    _(DEF._, "C1", _(AN._, "C"), _(AN._, "value", value("C1")))
            )
        );

        JExpression test = new JExpression(
            _(ANY._, "A", _(WITH._, "value", value("B")))
        );
        assertAnimoResultOneStep(test, "def B (A) (value \"B\").");
//        assertAnimoResultOneStep(test, "def B1 (B) (value \"B\").");

        test = new JExpression(
            _(ANY._, "A", _(WITH._, "value", value("C")))
        );
        assertAnimoResultOneStep(test, "def C (B) (value \"C\").");
//        assertAnimoResultOneStep(test, "def C1 (C) (value \"C\").");
    }

	@Test
	public void ANYwithEQ() throws Throwable {

		__(
            new JExpression(
                _(DEF._, "value-plain",
                    _(AN._, "mime-type"),
                    _(AN._, "value"),
                    _(AN._, "type", value("value/plain")),
                    _(AN._, "reference", value("Plain value")),
                    _(AN._, "extension", value("txt"))
                )
            ),
            new JExpression(
                _(DEF._, "application-atom",
                    _(AN._, "mime-type"),
                    _(AN._, "application"),
                    _(AN._, "type", value("application/atom+xml")),
                    _(AN._, "reference", value("Atom Feed Document")),
                    _(AN._, "extension", value("atom"))
                )
            )
        );

		JExpression test = new JExpression(
			_(ANY._, "mime-type", _(EQ._, "extension", value("txt")))
		);
		assertAnimoResultOneStep(test, "def value-plain (mime-type) (value) (type \"value/plain\") (reference \"Plain value\") (extension \"txt\").");
	}

	@Test
	public void test_01() throws Throwable {
		testAnimo("def x1 1.");
		testAnimo("def x2 2.");
		testAnimo("def y1 (z) (foo x1).");
		testAnimo("def y2 (z) (foo x2).");
		assertAnimoResult("any z with foo x2", "def y2 (z) (foo).");
	}

	@Test
	public void test_02() throws Throwable {
		testAnimo("def x1 (foo) 1.");
		testAnimo("def x2 (foo) 2.");
		testAnimo("def y1 (z) (x1).");
		testAnimo("def y2 (z) (x2).");
		assertAnimoResult("get foo y1", "x1.");		
		assertAnimoResult("any z with foo x2", "def y2 (z) (x2 (foo) 2).");
	}

    @Test
    public void test_03() throws Throwable {

        __(
            new JExpression(
                    _(DEF._, "A", _(AN._, "S"), _(AN._, "X", value("α")))
            ),
            new JExpression(
                    _(DEF._, "B", _(AN._, "A"), _(AN._, "Y", value("β")))
            ),
            new JExpression(
                    _(DEF._, "C", _(AN._, "B"), _(AN._, "Z", value("γ")))
            )
        );

        JExpression test = new JExpression(
            _(ANY._, "S", _(WITH._, "X", value("α")))
        );
        assertAnimoResultOneStep(test, "def A (S) (X \"α\").");
        

        test = new JExpression(
            _(ANY._, "S", _(WITH._, "Y", value("β")))
        );
        assertAnimoResultOneStep(test, "def B (A) (Y \"β\").");

        test = new JExpression(
            _(ANY._, "S", _(WITH._, "Z", value("γ")))
        );
        assertAnimoResultOneStep(test, "def C (B) (Z \"γ\").");
    }

    @Test
    public void test_04() throws Throwable {
        __(
            "def foo-site (site) (server-name \"foo.com\")",
            "def bar-site (site) (server-name \"bar.com\")",

            "def hello-foo (foo-site, root) (title \"hello foo\")",
            "def hello-bar (bar-site, root) (title \"hello bar\")",
            
            "def foo-root-layout (layout, foo, root) (\\h1)"
		);

        assertAnimoResult(
    		"any root with server-name \"foo.com\"", 
    		"def hello-foo (foo-site (site) (server-name)) (root) (title).");
    }

    @Test
    public void test_05() throws Throwable {

        __(
            "def foo-site (site) (server-name \"foo.com\") (weak-use foo)",
            "def bar-site (site) (server-name \"bar.com\") (weak-use bar)",

            "def hello-foo (foo-site, root) (title \"hello foo\")",
            "def hello-bar (bar-site, root) (title \"hello bar\")",
            
            "def foo-root-layout (layout, foo, root) (\\h1)"
        );

        assertAnimoResult("any xxx with server-name \"foo.com\"", "");
    }
}
