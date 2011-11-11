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

/**
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class AnotherMathTest extends ATest {

    @Test
	public void test_00() throws Exception {
        Expression e = new AnimoExpression("+ 1 2 3 4");
    	assertStringResult(e, "10");
	}

    @Test
	public void test_01() throws Exception {
        Expression e = new AnimoExpression("* 2 2");
    	assertStringResult(e, "4");
	}

    @Test
	public void test_02() throws Exception {
        Expression e = new AnimoExpression("* 2 2.0");
    	assertStringResult(e, "4.0");
	}

    @Test
	public void test_03() throws Exception {
        Expression e = new AnimoExpression("* 2.0 2");
    	assertStringResult(e, "4.0");
	}

    @Test
	public void test_04() throws Exception {
        Expression e = new AnimoExpression("* 2.0 2.0");
    	assertStringResult(e, "4.0");
	}

    @Test
	public void test_05() throws Exception {
        Expression e = new AnimoExpression("/ 4.0 2.0");
    	assertStringResult(e, "2.0");
	}

    @Test
	public void test_06() throws Exception {
        Expression e = new AnimoExpression("/ 4 2");
    	assertStringResult(e, "2");
	}

    @Test
	public void test_07() throws Exception {
        Expression e = new AnimoExpression("- 1 2 3.0 4");
    	assertStringResult(e, "-8.0");
	}

}