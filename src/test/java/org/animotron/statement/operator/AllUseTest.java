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
public class AllUseTest extends ATest {

    @Test
    public void simple_all_Use() throws Throwable {

        __(
            "def A (^S) (X 'α').",
            "def B (^A) (Y 'β').",
            "def C (^B) (Z 'γ') (X 'αα').",
            "def s all S."
        );

        assertAnimoResult("s use B", "s (B (A (S) (X \"α\")) (Y \"β\")) (C (B (A (S) (X \"α\")) (Y \"β\")) (Z \"γ\") (X \"αα\")).");
//        assertAnimoResult(test, "s (C (B (A (S) (X \"α\")) (Y \"β\")) (Z \"γ\") (X \"αα\")).");

//        assertAnimoResult(test, "s C (B) (Z \"γ\") (X \"αα\").");
        assertAnimoResult("s use C", "s C (B (A (S) (X \"α\")) (Y \"β\")) (Z \"γ\") (X \"αα\").");
    }

    @Test
    public void complex_all_Use() throws Throwable {

        __(
                "def A (^S) (X 'α').",
                "def B (^A) (Y 'β').",
                "def B1 (^B) (Y 'ββ').",
                "def C (^B) (Z 'γ') (X 'αα').",
                "def C1 (^C) (Z 'γγ') (X 'ααα').",
                "def s all S."
        );

        assertAnimoResult("def b s use B", "b s " +
    		"(B (A (S) (X \"α\")) (Y \"β\")) " +
    		"(B1 (B (A (S) (X \"α\")) (Y \"β\")) (Y \"ββ\")) " +
    		"(C (B (A (S) (X \"α\")) (Y \"β\")) (Z \"γ\") (X \"αα\")) " +
			"(C1 (C (B (A (S) (X \"α\")) (Y \"β\")) (Z \"γ\") (X \"αα\")) (Z \"γγ\") (X \"ααα\")).");

        assertAnimoResult("def c s use C", "c s " +
			"(C (B (A (S) (X \"α\")) (Y \"β\")) (Z \"γ\") (X \"αα\")) " +
			"(C1 (C (B (A (S) (X \"α\")) (Y \"β\")) (Z \"γ\") (X \"αα\")) (Z \"γγ\") (X \"ααα\")).");

        assertAnimoResult("def b s use C", "b s " +
			"(C (B (A (S) (X \"α\")) (Y \"β\")) (Z \"γ\") (X \"αα\")) " +
			"(C1 (C (B (A (S) (X \"α\")) (Y \"β\")) (Z \"γ\") (X \"αα\")) (Z \"γγ\") (X \"ααα\")).");
    }
}