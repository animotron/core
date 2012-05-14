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
import org.animotron.statement.query.ALL;
import org.junit.Test;

import static org.animotron.expression.JExpression._;
import static org.animotron.expression.AnimoExpression.__;
import static org.animotron.expression.JExpression.value;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class AllTest extends ATest {
	
    @Test
    public void testALL() throws Throwable {

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
            _(ALL._, "A")
        );
        assertAnimoResultOneStep(test, "the C (B) (value \"C\").");
    }
	
    @Test
    public void testALLwithoutTHE() throws Throwable {

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
            _(ALL._, "A")
        );
        assertAnimoResultOneStep(test, "the C (B) (value \"C\").");
        //assertAnimoResult(test, "the B\n    (A)\n    (value \"B\").\nthe C\n    (B)\n    (value \"C\").\n", true);
    }

    @Test
    public void testALLwithWITH() throws Throwable {

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
            _(ALL._, "A", _(WITH._, "value", value("B")))
        );
        assertAnimoResultOneStep(test, "the B1 (B) (value \"B\").");

        test = new JExpression(
            _(ALL._, "A", _(WITH._, "value", value("C")))
        );
        assertAnimoResultOneStep(test, "the C1 (C) (value \"C\").");
    }

	@Test
	public void ALLwithEQ() throws Throwable {

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
			_(ALL._, "mime-type", _(EQ._, "extension", value("txt")))
		);
        //assertAnimoResult(test, "the test the value-plain (mime-type) (value) (type \"value/plain\") (reference \"Plain value\") (extension \"txt\").");
		assertAnimoResultOneStep(test, "the value-plain (mime-type) (value) (type \"value/plain\") (reference \"Plain value\") (extension \"txt\").");
	}
	
	@Test
	public void test_00() throws Exception {
        __(
	        "the item1 (goods item) (qty (1) (kg)) (cost (10) (USD)).",
	        "the item2 (goods item) (qty (1) (kg)) (cost (5) (USD))."
        );

        assertAnimoResult(
            "all item",
            "the item1 (goods) (qty) (cost). the item2 (goods) (qty) (cost)."
        );
	}

    @Test
    //TODO is all select a closest not leaf by a predicate?
    public void test_01() throws Throwable {

    	__(
            new JExpression(
                    _(THE._, "A", _(AN._, "S"), _(AN._, "X", value("α")))
            ),
            new JExpression(
                    _(THE._, "B", _(AN._, "A"), _(AN._, "Y", value("β")))
            ),
            new JExpression(
                    _(THE._, "C", _(AN._, "B"), _(AN._, "Z", value("γ")), _(AN._, "X", value("αα")))
            )
        );

        JExpression a = new JExpression(
            _(THE._, "a", _(ALL._, "S", _(WITH._, "X", value("α"))))
        );
        //assertAnimoResultOneStep(a, "the a the B (A) (Y \"β\").");
        assertAnimoResultOneStep(a, "the a.");

        JExpression b = new JExpression(
            _(THE._, "b", _(ALL._, "S", _(WITH._, "Y", value("β"))))
        );
        assertAnimoResultOneStep(b, "the b the C (B) (Z \"γ\") (X \"αα\").");

        JExpression c = new JExpression(
            _(THE._, "c", _(ALL._, "S", _(WITH._, "Z", value("γ"))))
        );
        assertAnimoResultOneStep(c, "the c the C (B) (Z \"γ\") (X \"αα\").");

    }
}