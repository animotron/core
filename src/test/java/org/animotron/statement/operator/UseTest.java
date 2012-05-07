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
import org.animotron.statement.query.ANY;
import org.animotron.statement.relation.USE;
import org.junit.Test;

import static org.animotron.expression.JExpression._;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class UseTest extends ATest {

    @Test
    public void any_use() throws Throwable {

        new JExpression(
                _(THE._, "A", _(AN._, "X"))
        );

        JExpression x = new JExpression(
            _(THE._, "x", _(ANY._, "X", _(USE._, "Y")))
        );
        assertAnimoResult(x, "the x the A X.");
    }

    @Test
    public void an_any_use() throws Throwable {

        new JExpression(
                _(THE._, "A", _(AN._, "X"))
        );
        new JExpression(
                _(THE._, "q", _(ANY._, "X"))
        );

        JExpression x = new JExpression(
            _(THE._, "x", _(AN._, "q", _(USE._, "Y")))
        );
        assertAnimoResult(x, "the x q the A X.");
    }

    @Test
    public void all_use() throws Throwable {

        new JExpression(
                _(THE._, "A", _(AN._, "X"))
        );
        new JExpression(
                _(THE._, "B", _(AN._, "X"))
        );

        JExpression x = new JExpression(
            _(THE._, "x", _(ALL._, "X", _(USE._, "Y")))
        );
        assertAnimoResult(x, "the x (the A X) (the B X).");
    }

    @Test
    public void an_all_use() throws Throwable {

        new JExpression(
                _(THE._, "A", _(AN._, "X"))
        );
        new JExpression(
                _(THE._, "B", _(AN._, "X"))
        );
        new JExpression(
                _(THE._, "q", _(ALL._, "X"))
        );

        JExpression x = new JExpression(
            _(THE._, "x", _(AN._, "q", _(USE._, "Y")))
        );
        assertAnimoResult(x, "the x q (the A X) (the B X).");
    }
    
    @Test
    public void cross_use_case() throws Throwable {

        new JExpression(
                _(THE._, "A", _(AN._, "S"), _(AN._, "X"))
        );
        new JExpression(
                _(THE._, "B", _(AN._, "S"), _(AN._, "Y"))
        );
        new JExpression(
                _(THE._, "C", _(AN._, "S"), _(AN._, "X"), _(AN._, "Y"))
        );

        JExpression test;
        test = new JExpression(
            _(ALL._, "S")
        );
        assertAnimoResult(test, "the A (S) (X). the B (S) (Y). the C (S) (X) (Y).");

        test = new JExpression(
            _(ALL._, "S", _(USE._, "X"))
        );
        assertAnimoResult(test, "the A (S) (X). the C (S) (X) (Y).");

        test = new JExpression(
            _(ALL._, "S", _(USE._, "Y"))
        );
        assertAnimoResult(test, "the B (S) (Y). the C (S) (X) (Y).");

        test = new JExpression(
            _(ALL._, "S", _(USE._, "X"), _(USE._, "Y"))
        );
        assertAnimoResult(test, "the C (S) (X) (Y).");

    }    
}