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

import static org.animotron.expression.AnimoExpression.__;


/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class UseTest extends ATest {

    @Test
    public void any_use() throws Throwable {

        __(
                "def A X"
        );

        Expression x = new AnimoExpression("def x any X use Y");
        assertAnimoResult(x, "x A X.");
    }

    @Test
    public void an_any_use() throws Throwable {

        __(
                "def A X",
                "def q any X"
        );

        Expression x = new AnimoExpression("def x q use Y");
        assertAnimoResult(x, "x q A X.");
    }

    @Test
    public void all_use() throws Throwable {

        __(
                "def A X",
                "def B X"
        );

        Expression x = new AnimoExpression("def x all X use Y");
        assertAnimoResult(x, "x (A X) (B X).");
    }

    @Test
    public void an_all_use() throws Throwable {

        __(
                "def A X",
                "def B X",
                "def q all X"
        );

        Expression x = new AnimoExpression("def x q use Y");
        assertAnimoResult(x, "x q (A X) (B X).");
    }
    
    @Test
    public void cross_use_case() throws Throwable {

        __(
                "def A (S) (X)",
                "def B (S) (Y)",
                "def C (S) (X) (Y)"
        );

        Expression test;

        test = new AnimoExpression("all S");
        assertAnimoResult(test, "A (S) (X). B (S) (Y). C (S) (X) (Y).");

        test = new AnimoExpression("all S use X");
        assertAnimoResult(test, "A (S) (X). C (S) (X) (Y).");

        test = new AnimoExpression("all S use Y");
        assertAnimoResult(test, "B (S) (Y). C (S) (X) (Y).");

        test = new AnimoExpression("all S (use X) (use Y)");
        assertAnimoResult(test, "C (S) (X) (Y).");

    }    
}