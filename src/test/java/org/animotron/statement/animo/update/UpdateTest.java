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
package org.animotron.statement.animo.update;

import org.animotron.ATest;
import org.animotron.expression.AnimoExpression;
import org.animotron.expression.Expression;
import org.junit.Test;

/**
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class UpdateTest extends ATest {

    @Test
	public void test_00() throws Exception {
        Expression e = new AnimoExpression("the a have x 1.");
        eval(new AnimoExpression("add a have y 2."));
        assertAnimo(e, "the a (have x 1) (have y 2).");
	}

    @Test
	public void test_01() throws Exception {
        Expression e = new AnimoExpression("the a have x 1.");
        eval(new AnimoExpression("replace (get x a) (have y 2)."));
        assertAnimo(e, "the a have y 2.");
	}

    @Test
	public void test_02() throws Exception {
        Expression e = new AnimoExpression("the a have x 1.");
        eval(new AnimoExpression("set (get x a) (2)."));
        assertAnimo(e, "the a have x 2.");
	}

    @Test
	public void test_03() throws Exception {
        Expression a = new AnimoExpression("the a have x 1.");
        Expression b = new AnimoExpression("the b have x 1.");
        Expression c = new AnimoExpression("the c have x 1.");
        eval(new AnimoExpression("set (get x a) (2)."));
        assertAnimo(a, "the a have x 2.");
        assertAnimo(b, "the b have x 1.");
        assertAnimo(c, "the c have x 1.");
	}

    @Test
	public void test_04() throws Exception {
        Expression a = new AnimoExpression("the a have x 1.");
        Expression b = new AnimoExpression("the b have x 1.");
        Expression c = new AnimoExpression("the c have x 1.");
        eval(new AnimoExpression("replace (get x a) (have y 2)."));
        assertAnimo(a, "the a have y 2.");
        assertAnimo(b, "the b have x 1.");
        assertAnimo(c, "the c have x 1.");
	}

}