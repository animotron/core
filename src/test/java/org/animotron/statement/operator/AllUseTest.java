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
import org.animotron.statement.query.ALL;
import org.animotron.statement.relation.USE;
import org.junit.Ignore;
import org.junit.Test;

import static org.animotron.expression.JExpression._;
import static org.animotron.expression.JExpression.value;
import static org.animotron.expression.JExpression.element;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class AllUseTest extends ATest {

    @Test
    public void simple_all_Use() throws Exception {

        JExpression.__(
                new JExpression(
                        _(THE._, "A", _(AN._, "S"), element("X", value("α")))
                ),
                new JExpression(
                        _(THE._, "B", _(AN._, "A"), element("Y", value("β")))
                ),
                new JExpression(
                        _(THE._, "C", _(AN._, "B"), element("Z", value("γ")), element("X", value("αα")))
                ),
                new JExpression(
                        _(THE._, "s", _(ALL._, "S"))
                )
        );

        JExpression test = new JExpression(
            _(AN._, "s", _(USE._, "B"))
        );
        //assertAnimoResult(test, "the s (the B (A) (Y \"β\")) (the C (B) (Z \"γ\") (X \"αα\")).");
        assertAnimoResult(test, "s the C (B (A (S) (\\X \"α\")) (\\Y \"β\")) (\\Z \"γ\") (\\X \"αα\").");

        test = new JExpression(
            _(AN._, "s", _(USE._, "C"))
        );
        //assertAnimoResult(test, "the s the C (B) (Z \"γ\") (X \"αα\").");
        assertAnimoResult(test, "s the C (B (A (S) (\\X \"α\")) (\\Y \"β\")) (\\Z \"γ\") (\\X \"αα\").");
    }

    @Test
    @Ignore
    public void simple_all_Use_1() throws Exception {

        JExpression.__(
                new JExpression(
                        _(THE._, "A", _(AN._, "S"), _(AN._, "X", value("α")))
                ),
                new JExpression(
                        _(THE._, "B", _(AN._, "A"), _(AN._, "Y", value("β")))
                ),
                new JExpression(
                        _(THE._, "C", _(AN._, "B"), _(AN._, "Z", value("γ")), _(AN._, "X", value("αα")))
                ),
                new JExpression(
                        _(THE._, "s", _(ALL._, "S"))
                ),
                new JExpression(
                        _(THE._, "ub", _(USE._, "B"))
                ),
                new JExpression(
                        _(THE._, "uc", _(USE._, "C"))
                )
        );

        JExpression b = new JExpression(
            _(THE._, "b", _(AN._, "s", _(AN._, "ub")))
        );
        assertAnimoResult(b, "the b the s (the B (A) (Y \"β\")) (the C (B) (Z \"γ\") (X \"αα\")).");

        JExpression c = new JExpression(
            _(THE._, "c", _(AN._, "s", _(AN._, "uc")))
        );
        assertAnimoResult(c, "the c the s the C (B) (Z \"γ\") (X \"αα\").");

    }

    @Test
    public void complex_all_Use() throws Exception {

        JExpression.__(
                new JExpression(
                        _(THE._, "A", _(AN._, "S"), element("X", value("α")))
                ),
                new JExpression(
                        _(THE._, "B", _(AN._, "A"), element("Y", value("β")))
                ),
                new JExpression(
                        _(THE._, "B1", _(AN._, "B"), element("Y", value("ββ")))
                ),
                new JExpression(
                        _(THE._, "C", _(AN._, "B"), element("Z", value("γ")), element("X", value("αα")))
                ),
                new JExpression(
                        _(THE._, "C1", _(AN._, "C"), element("Z", value("γγ")), element("X", value("ααα")))
                ),
                new JExpression(
                        _(THE._, "s", _(ALL._, "S"))
                )
        );

        JExpression b = new JExpression(
            _(THE._, "b", _(AN._, "s", _(USE._, "B")))
        );
        assertAnimoResult(b, "the b s " +
        		"(the B1 (B (A (S) (\\X \"α\")) (\\Y \"β\")) (\\Y \"ββ\")) " +
        		"(the C1 (C (B (A (S) (\\X \"α\")) (\\Y \"β\")) (\\Z \"γ\") (\\X \"αα\")) (\\Z \"γγ\") (\\X \"ααα\")).");

        JExpression c = new JExpression(
            _(THE._, "c", _(AN._, "s", _(USE._, "C")))
        );
        assertAnimoResult(c, "the c s " +
        		"the C1 (C (B (A (S) (\\X \"α\")) (\\Y \"β\")) (\\Z \"γ\") (\\X \"αα\")) (\\Z \"γγ\") (\\X \"ααα\").");
    }

    @Test
    @Ignore
    public void complex_all_Use_1() throws Exception {

        JExpression.__(
                new JExpression(
                        _(THE._, "A", _(AN._, "S"), _(AN._, "X", value("α")))
                ),
                new JExpression(
                        _(THE._, "B", _(AN._, "A"), _(AN._, "Y", value("β")))
                ),
                new JExpression(
                        _(THE._, "B1", _(AN._, "B"), _(AN._, "Y", value("ββ")))
                ),
                new JExpression(
                        _(THE._, "C", _(AN._, "B"), _(AN._, "Z", value("γ")), _(AN._, "X", value("αα")))
                ),
                new JExpression(
                        _(THE._, "C1", _(AN._, "C"), _(AN._, "Z", value("γγ")), _(AN._, "X", value("ααα")))
                ),
                new JExpression(
                        _(THE._, "s", _(ALL._, "S"))
                ),
                new JExpression(
                        _(THE._, "ub", _(USE._, "B"))
                ),
                new JExpression(
                        _(THE._, "uc", _(USE._, "C"))
                )
        );

        JExpression b = new JExpression(
            _(THE._, "b", _(AN._, "s", _(AN._, "ub")))
        );
        assertAnimoResult(b, "the b the s (the B (A) (Y \"β\")) (the B1 (B) (Y \"ββ\")) (the C (B) (Z \"γ\") (X \"αα\")) (the C1 (C) (Z \"γγ\") (X \"ααα\")).");

        JExpression c = new JExpression(
            _(THE._, "c", _(AN._, "s", _(AN._, "uc")))
        );
        assertAnimoResult(c, "the c the s (the C (B) (Z \"γ\") (X \"αα\")) (the C1 (C) (Z \"γγ\") (X \"ααα\")).");
    }
}
