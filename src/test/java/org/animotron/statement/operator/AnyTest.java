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

import static org.animotron.expression.Expression.__;
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
                    _(THE._, "A", _(AN._, "value"))
            ),
            new JExpression(
                    _(THE._, "B", _(AN._, "A"), _(AN._, "value", value("B")))
            ),
            new JExpression(
                    _(THE._, "C", _(AN._, "B"), _(AN._, "value", value("C")))
            )
        );

        JExpression test = new JExpression(
            _(ANY._, "A")
        );
        assertAnimoResultOneStep(test, "the C (B) (value \"C\").");

        test = new JExpression(
            _(ANY._, "B")
        );
        assertAnimoResultOneStep(test, "the C (B) (value \"C\").");
    }
	
    @Test
    public void testANYwithWITH() throws Throwable {

        __(
            new JExpression(
                    _(THE._, "A", _(AN._, "value"))
            ),
            new JExpression(
                    _(THE._, "B", _(AN._, "A"), _(AN._, "value", value("B")))
            ),
            new JExpression(
                    _(THE._, "B1", _(AN._, "B"), _(AN._, "value", value("B")))
            ),
            new JExpression(
                    _(THE._, "C", _(AN._, "B"), _(AN._, "value", value("C")))
            ),
            new JExpression(
                    _(THE._, "C1", _(AN._, "C"), _(AN._, "value", value("C")))
            )
        );

        JExpression test = new JExpression(
            _(ANY._, "A", _(WITH._, "value", value("B")))
        );
        assertAnimoResultOneStep(test, "the B1 (B) (value \"B\").");

        test = new JExpression(
            _(ANY._, "A", _(WITH._, "value", value("C")))
        );
        assertAnimoResultOneStep(test, "the C1 (C) (value \"C\").");
    }

	@Test
	public void ANYwithEQ() throws Throwable {

		__(
            new JExpression(
                _(THE._, "value-plain",
                    _(AN._, "mime-type"),
                    _(AN._, "value"),
                    _(AN._, "type", value("value/plain")),
                    _(AN._, "reference", value("Plain value")),
                    _(AN._, "extension", value("txt"))
                )
            ),
            new JExpression(
                _(THE._, "application-atom",
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
		assertAnimoResultOneStep(test, "the value-plain (mime-type) (value) (type \"value/plain\") (reference \"Plain value\") (extension \"txt\").");
	}

	@Test
	public void test_01() throws Throwable {
		testAnimo("the x1 1.");
		testAnimo("the x2 2.");
		testAnimo("the y1 (z) (foo x1).");
		testAnimo("the y2 (z) (foo x2).");
		assertAnimoResult("any z with foo x2", "the y2 (z) (foo).");		
	}

	@Test
	public void test_02() throws Throwable {
		testAnimo("the x1 (foo) 1.");
		testAnimo("the x2 (foo) 2.");
		testAnimo("the y1 (z) (x1).");
		testAnimo("the y2 (z) (x2).");
		assertAnimoResult("get foo y1", "x1.");		
		assertAnimoResult("any z with foo x2", "the y2 (z) (x2 (foo) 2).");		
	}

    @Test
    public void test_03() throws Throwable {

        __(
            new JExpression(
                    _(THE._, "A", _(AN._, "S"), _(AN._, "X", value("α")))
            ),
            new JExpression(
                    _(THE._, "B", _(AN._, "A"), _(AN._, "Y", value("β")))
            ),
            new JExpression(
                    _(THE._, "C", _(AN._, "B"), _(AN._, "Z", value("γ")))
            )
        );

        JExpression test = new JExpression(
            _(THE._, "a", _(ANY._, "S", _(WITH._, "X", value("α"))))
        );
        //assertAnimoResultOneStep(a, "the a the A (S) (X \"α\").");
        assertAnimoResultOneStep(test, "the a.");

        test = new JExpression(
            _(THE._, "b", _(ANY._, "S", _(WITH._, "Y", value("β"))))
        );
        //assertAnimoResultOneStep(b, "the b the B (A) (Y \"β\").");
        assertAnimoResultOneStep(test, "the b.");

        test = new JExpression(
            _(THE._, "c", _(ANY._, "S", _(WITH._, "Z", value("γ"))))
        );
        assertAnimoResultOneStep(test, "the c the C (B) (Z \"γ\").");
    }
}
