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
import org.animotron.statement.compare.WITH;
import org.animotron.statement.query.ANY;
import org.junit.Ignore;
import org.junit.Test;

import static org.animotron.expression.JExpression._;
import static org.animotron.expression.JExpression.__;
import static org.animotron.expression.JExpression.value;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class YetAnotherAnyTest extends ATest {

    @Test
    public void deep_any() throws Exception {

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
        assertAnimoResultOneStep(test, "the a the A (S) (X \"α\").");

        test = new JExpression(
            _(THE._, "b", _(ANY._, "S", _(WITH._, "Y", value("β"))))
        );
        //assertAnimoResultOneStep(b, "the b the B (A) (Y \"β\").");
        assertAnimoResultOneStep(test, "the b the B (A) (Y \"β\").");

        test = new JExpression(
            _(THE._, "c", _(ANY._, "S", _(WITH._, "Z", value("γ"))))
        );
        assertAnimoResultOneStep(test, "the c the C (B) (Z \"γ\").");
    }

    @Test
    @Ignore
    //TODO is all select a closest not leaf by a predicate?
    public void one_more_deep_any() throws Exception {

        __(
            new JExpression(
                    _(THE._, "A", _(AN._, "S"), _(AN._, "X", value("α")))
            ),
            new JExpression(
                    _(THE._, "B", _(AN._, "A"), _(AN._, "X", value("β")))
            ),
            new JExpression(
                    _(THE._, "C", _(AN._, "B"), _(AN._, "X", value("γ")))
            )
        );

        JExpression test = new JExpression(
            _(THE._, "a", _(ANY._, "S", _(WITH._, "X", value("α"))))
        );
        assertAnimoResultOneStep(test, "the a the A (S) (X \"α\").");

        test = new JExpression(
            _(THE._, "b", _(ANY._, "S", _(WITH._, "X", value("β"))))
        );
        assertAnimoResultOneStep(test, "the b the B (A) (X \"β\").");

        test = new JExpression(
            _(THE._, "c", _(ANY._, "S", _(WITH._, "X", value("γ"))))
        );
        assertAnimoResultOneStep(test, "the c the C (B) (X \"γ\").");
    }
}