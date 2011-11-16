/*
 *  Copyright (C) 2011 The Animo Project
 *  http://animotron.org
 *
 *  This program is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public License
 *  as published by the Free Software Foundation; either version 3
 *  of the License, or (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */
package org.animotron.statement.math;

import org.animotron.ATest;
import org.animotron.expression.AnimoExpression;
import org.animotron.expression.Expression;
import org.junit.Test;

import static org.animotron.expression.Expression.__;

/**
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class YetAnotherMathTest extends ATest {

    @Test
	public void test_00() throws Exception {
        __(
            new AnimoExpression("the a 1"),
            new AnimoExpression("the b 2"),
            new AnimoExpression("the c 3"),
            new AnimoExpression("the d 4")
        );
        Expression e = new AnimoExpression("+ (a) (b) (c) (d)");
    	assertStringResult(e, "10");
	}

    @Test
	public void test_01() throws Exception {
        __(
            new AnimoExpression("the a (is z) (1)"),
            new AnimoExpression("the b (is z) (2)"),
            new AnimoExpression("the c (is z) (3)"),
            new AnimoExpression("the d (is z) (4)")
        );
        Expression e = new AnimoExpression("+ all z");
    	assertStringResult(e, "10");
	}

    @Test
	public void test_02() throws Exception {
        __(
            new AnimoExpression("the a have x 1"),
            new AnimoExpression("the b have x 2"),
            new AnimoExpression("the c have x 3"),
            new AnimoExpression("the d have x 4")
        );
        Expression e = new AnimoExpression("+ get x (a) (b) (c) (d)");
    	assertStringResult(e, "10");
	}

    @Test
	public void test_03() throws Exception {
        __(
            new AnimoExpression("the a (is z) (have x 1)"),
            new AnimoExpression("the b (is z) (have x 2)"),
            new AnimoExpression("the c (is z) (have x 3)"),
            new AnimoExpression("the d (is z) (have x 4)")
        );
        Expression e = new AnimoExpression("+ get x all z");
    	assertStringResult(e, "10");
	}

}