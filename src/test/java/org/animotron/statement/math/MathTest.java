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
import org.animotron.expression.Expression;
import org.animotron.expression.JExpression;
import org.junit.Test;

import static org.animotron.expression.JExpression._;
import static org.animotron.expression.JExpression.value;

/**
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class MathTest extends ATest {

    @Test
	public void test_performace() throws Exception {
        Expression e = new JExpression(
            _(SUM._, value(1), value(2), value(3), value(4))
        );
        
        long time = System.currentTimeMillis();
        for (int i = 0; i<10000;i++) {
            time = System.currentTimeMillis();
        	assertStringResult(e, "10", false);
        	System.out.println(System.currentTimeMillis() - time);
        }
	}
	@Test
	public void test_00() throws Exception {
        Expression e = new JExpression(
            _(SUM._, value(1), value(2), value(3), value(4))
        );
    	assertStringResult(e, "10");
	}

    @Test
	public void test_01() throws Exception {
        Expression e = new JExpression(
            _(MUL._, value(2), value(2))
        );
    	assertStringResult(e, "4");
	}

    @Test
	public void test_02() throws Exception {
        Expression e = new JExpression(
            _(MUL._, value(2), value(2.0))
        );
    	assertStringResult(e, "4.0");
	}

    @Test
	public void test_03() throws Exception {
        Expression e = new JExpression(
            _(MUL._, value(2.0), value(2))
        );
    	assertStringResult(e, "4.0");
	}

    @Test
	public void test_04() throws Exception {
        Expression e = new JExpression(
            _(MUL._, value(2.0), value(2.0))
        );
    	assertStringResult(e, "4.0");
	}

    @Test
	public void test_05() throws Exception {
        Expression e = new JExpression(
            _(DIV._, value(4.0), value(2.0))
        );
    	assertStringResult(e, "2.0");
	}

    @Test
	public void test_06() throws Exception {
        Expression e = new JExpression(
            _(DIV._, value(4), value(2))
        );
    	assertStringResult(e, "2.0");
	}

    @Test
	public void test_07() throws Exception {
        Expression e = new JExpression(
            _(SUB._, value(1), value(2), value(3.0), value(4))
        );
    	assertStringResult(e, "-8.0");
	}

    @Test
	public void test_08() throws Exception {
        Expression e = new JExpression(
            _(SUM._, value("1"), value("2"), value("3.0"), value("4"))
        );
    	assertStringResult(e, "10.0");
	}

}