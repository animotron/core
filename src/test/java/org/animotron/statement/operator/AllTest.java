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
import org.animotron.expression.AnimoExpression;
import org.animotron.expression.Expression;
import org.junit.Test;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class AllTest extends ATest {
	
    @Test
    public void testALL() throws Throwable {

        __(
                "def A v.",
                "def B (^A) (v 'B').",
                "def C (^B) (v 'C')."
        );

        Expression test = new AnimoExpression("all A");
        assertAnimoResultOneStep(test, "B (^A) (v \"B\"). C (^B) (v \"C\").");
    }

    @Test
    public void testALLwithWITH() throws Throwable {

        __(
                "def A v.",
                "def B (^A) (v 'B').",
                "def B1 (^B) (v 'B').",
                "def C (^B) (v 'C').",
                "def C1 (^C) (v 'C')."
        );

        Expression test = new AnimoExpression("all A with v 'B'");
        assertAnimoResultOneStep(test, "B (^A) (v \"B\"). B1 (^B) (v \"B\").");

        test = new AnimoExpression("all A with v 'C'");
        assertAnimoResultOneStep(test, "C (^B) (v \"C\"). C1 (^C) (v \"C\").");
    }

	@Test
	public void ALLwithEQ() throws Throwable {

        __(
                "def v-plain (mime-type) (v) (type 'v/plain') (reference 'Plain v') (extension 'txt').",
                "def application-atom (mime-type) (type 'application/atom+xml') (reference 'Atom Feed Document') (extension 'atom')."
        );

		assertAnimoResultOneStep(
				"all mime-type eq (extension) ('txt')", 
				"v-plain (mime-type) (v) (type \"v/plain\") (reference \"Plain v\") (extension \"txt\").");
	}

	@Test
	public void test_00() throws Exception {
        __(
	        "def item1 (item) (qty 1 (kg)) (cost 10 (USD)).",
	        "def item2 (item) (qty 1 (kg)) (cost 5 (USD))."
        );

        assertAnimoResult(
            "all item",
            "item1 (item) (qty) (cost). item2 (item) (qty) (cost)."
        );
	}

    @Test
    //TODO is all select a closest not leaf by a predicate?
    public void test_01() throws Throwable {

    	__(
            "def A (^S) (X 'α').",
            "def B (^A) (Y 'β').",
            "def C (^B) (Z 'γ') (X 'αα')."
        );

        assertAnimoResultOneStep("all S with X 'α'", "A (^S) (X \"α\"). B (^A) (Y \"β\").");

        assertAnimoResultOneStep("all S with Y 'β'", "B (^A) (Y \"β\"). C (^B) (Z \"γ\") (X \"αα\").");

        assertAnimoResultOneStep("all S with Z 'γ'", "C (^B) (Z \"γ\") (X \"αα\").");
    }
}