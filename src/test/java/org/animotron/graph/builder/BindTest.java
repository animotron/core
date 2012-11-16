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
package org.animotron.graph.builder;

import org.animotron.ATest;
import org.animotron.expression.AbstractExpression;
import org.animotron.expression.AnimoExpression;
import org.animotron.expression.Expression;
import org.animotron.statement.operator.AN;
import org.animotron.statement.operator.DEF;
import org.animotron.statement.operator.REF;
import org.junit.Test;

/**
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class BindTest extends ATest {

    @Test
	public void test_01() throws Throwable {
        final Expression a = new AnimoExpression("a");
        Expression b = new AbstractExpression() {
            @Override
            public void build() throws Throwable {
                builder.start(DEF._, "b");
                    builder.bind(a);
                builder.end();
            }
        };
        assertAnimo(b, "def b a.");
	}

    @Test
	public void test_02() throws Throwable {
        final Expression a = new AnimoExpression("a");
        Expression b = new AbstractExpression() {
            @Override
            public void build() throws Throwable {
                builder.start(DEF._, "b");
                    builder.start(AN._);
                        builder._(REF._,  "x");
                    builder.end();
                    builder.bind(a);
                builder.end();
            }
        };
        assertAnimo(b, "def b (x) (a).");
	}

    @Test
	public void test_03() throws Throwable {
        final Expression x = new AnimoExpression("def x");
        final Expression y = new AnimoExpression("def y");
        Expression b = new AbstractExpression() {
            @Override
            public void build() throws Throwable {
                builder.start(DEF._, "b");
                    builder.start(AN._);
                        builder._(REF._,  x);
                    builder.end();
                    builder.start(AN._);
                        builder._(REF._,  y);
                    builder.end();
                builder.end();
            }
        };
        assertAnimo(b, "def b (x) (y).");
	}

    @Test
	public void test_04() throws Throwable {
        final Expression a = new AnimoExpression("any a");
        Expression b = new AbstractExpression() {
            @Override
            public void build() throws Throwable {
                builder.start(DEF._, "b");
                    builder.start(AN._);
                        builder._(REF._,  "x");
                        builder.bind(a);
                    builder.end();
                builder.end();
            }
        };
        assertAnimo(b, "def b y any a.");
	}

    @Test
	public void test_06() throws Throwable {
        final Expression a = new AnimoExpression("any a");
        final Expression x = new AnimoExpression("def x");
        final Expression y = new AnimoExpression("def y");
        Expression b = new AbstractExpression() {
            @Override
            public void build() throws Throwable {
                builder.start(DEF._, "b");
                    builder.start(AN._);
                        builder._(REF._,  x);
                    builder.end();
                    builder.start(AN._);
                        builder._(REF._, y);
                        builder.bind(a);
                    builder.end();
                builder.end();
            }
        };
        assertAnimo(b, "def b (x) (y any a).");
	}

}
