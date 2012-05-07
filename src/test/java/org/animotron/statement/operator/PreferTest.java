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
import org.animotron.statement.query.PREFER;
import org.animotron.statement.relation.USE;
import org.junit.Test;

import static org.animotron.expression.JExpression._;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class PreferTest extends ATest {


    @Test
    public void test_0() throws Throwable {

        new JExpression(
                _(THE._, "A", _(AN._, "X"))
        );
        new JExpression(
                _(THE._, "B", _(AN._, "X"))
        );

        JExpression test = new JExpression(
                _(PREFER._, "X")
        );
        assertAnimoResultOneStep(test, "");

    }

    @Test
    public void test_01() throws Throwable {

        new JExpression(
                _(THE._, "A", _(AN._, "X"))
        );
        new JExpression(
                _(THE._, "B", _(AN._, "X"))
        );

        JExpression test = new JExpression(
                _(PREFER._, "X", _(USE._, "X"))
        );
        assertAnimoResultOneStep(test, "the A X. the B X.");

    }

    @Test
    public void test_02() throws Throwable {

        new JExpression(
                _(THE._, "A", _(AN._, "X"))
        );
        new JExpression(
                _(THE._, "B", _(AN._, "X"))
        );

        JExpression test = new JExpression(
            _(PREFER._, "X", _(USE._, "A"))
        );
        assertAnimoResultOneStep(test, "the A X.");

    }

    @Test
    public void test_03() throws Throwable {

        new JExpression(
                _(THE._, "A", _(AN._, "X"))
        );
        new JExpression(
                _(THE._, "B", _(AN._, "X"))
        );

        JExpression test = new JExpression(
                _(PREFER._, "X", _(USE._, "B"))
        );
        assertAnimoResultOneStep(test, "the B X.");
    }
}