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

import static org.animotron.expression.AnimoExpression.__;
import static org.animotron.expression.JExpression._;
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
                _(DEF._, "A", _(AN._, "value"))
            ),
            new JExpression(
                _(DEF._, "B", _(NONSTOP._, "A"), _(AN._, "value", value("B")))
            ),
            new JExpression(
                _(DEF._, "C", _(NONSTOP._, "B"), _(AN._, "value", value("C")))
            )
        );

        JExpression test = new JExpression(
            _(ALL._, "A")
        );
        assertAnimoResultOneStep(test, "B (^A) (value \"B\"). C (^B) (value \"C\").");
    }
	
    @Test
    public void testALLwithoutTHE() throws Throwable {

        __(
            new JExpression(
                _(DEF._, "A", _(AN._, "value"))
            ),
            new JExpression(
                _(DEF._, "B", _(NONSTOP._, "A"), _(AN._, "value", value("B")))
            ),
            new JExpression(
                _(DEF._, "C", _(NONSTOP._, "B"), _(AN._, "value", value("C")))
            )
        );

        JExpression test = new JExpression(
            _(ALL._, "A")
        );
        assertAnimoResultOneStep(test, "B (^A) (value \"B\"). C (^B) (value \"C\").");
    }

    @Test
    public void testALLwithWITH() throws Throwable {

        __(
            new JExpression(
                _(DEF._, "A", _(AN._, "value"))
            ),
            new JExpression(
                _(DEF._, "B", _(NONSTOP._, "A"), _(AN._, "value", value("B")))
            ),
            new JExpression(
                _(DEF._, "B1", _(NONSTOP._, "B"), _(AN._, "value", value("B")))
            ),
            new JExpression(
                _(DEF._, "C", _(NONSTOP._, "B"), _(AN._, "value", value("C")))
            ),
            new JExpression(
                _(DEF._, "C1", _(NONSTOP._, "C"), _(AN._, "value", value("C")))
            )
        );

        JExpression test = new JExpression(
            _(ALL._, "A", _(WITH._, "value", value("B")))
        );
        assertAnimoResultOneStep(test, "B (^A) (value \"B\"). B1 (^B) (value \"B\").");

        test = new JExpression(
            _(ALL._, "A", _(WITH._, "value", value("C")))
        );
        assertAnimoResultOneStep(test, "C (^B) (value \"C\"). C1 (^C) (value \"C\").");
    }

	@Test
	public void ALLwithEQ() throws Throwable {

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
			_(ALL._, "mime-type", _(EQ._, "extension", value("txt")))
		);
        //assertAnimoResult(test, "test the value-plain (mime-type) (value) (type \"value/plain\") (reference \"Plain value\") (extension \"txt\").");
		assertAnimoResultOneStep(test, "value-plain (mime-type) (value) (type \"value/plain\") (reference \"Plain value\") (extension \"txt\").");
	}
	
	@Test
	public void test_00() throws Exception {
        __(
	        "def item1 (goods item) (qty (1) (kg)) (cost (10) (USD)).",
	        "def item2 (goods item) (qty (1) (kg)) (cost (5) (USD))."
        );

        assertAnimoResult(
            "all item",
            " item1 (goods) (qty) (cost).  item2 (goods) (qty) (cost)."
        );
	}

    @Test
    //TODO is all select a closest not leaf by a predicate?
    public void test_01() throws Throwable {

    	__(
            new JExpression(
                    _(DEF._, "A", _(NONSTOP._, "S"), _(AN._, "X", value("α")))
            ),
            new JExpression(
                    _(DEF._, "B", _(NONSTOP._, "A"), _(AN._, "Y", value("β")))
            ),
            new JExpression(
                    _(DEF._, "C", _(NONSTOP._, "B"), _(AN._, "Z", value("γ")), _(AN._, "X", value("αα")))
            )
        );

        JExpression test = new JExpression(
            _(ALL._, "S", _(WITH._, "X", value("α")))
        );
        assertAnimoResultOneStep(test, "A (^S) (X \"α\"). B (^A) (Y \"β\").");

        test = new JExpression(
            _(ALL._, "S", _(WITH._, "Y", value("β")))
        );
        assertAnimoResultOneStep(test, "B (^A) (Y \"β\"). C (^B) (Z \"γ\") (X \"αα\").");

        test = new JExpression(
            _(ALL._, "S", _(WITH._, "Z", value("γ")))
        );
        assertAnimoResultOneStep(test, "C (^B) (Z \"γ\") (X \"αα\").");

    }
}