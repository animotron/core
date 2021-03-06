/*
 *  Copyright (C) 2011-2013 The Animo Project
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
package org.animotron.statement.math;

import org.animotron.ATest;
import org.animotron.expression.Expression;
import org.animotron.expression.AnimoExpression;
import org.animotron.expression.Expression;
import org.animotron.statement.operator.AN;
import org.animotron.statement.operator.REF;
import org.junit.Test;
import org.neo4j.graphdb.Relationship;

import static java.lang.System.currentTimeMillis;

/**
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class SUM500Test extends ATest {

	@Test
	public void test_00() throws Throwable {
        long t;
        t = currentTimeMillis();
        Relationship e = AnimoExpression.__(new Expression() {
            @Override
            public void build() throws Throwable {
                builder.start(AN._);
                builder._(REF._, SUM._.name());
                for (int i = 0; i < 500; i++) {
                    builder._(i);
                }
                builder.end();
            }
        });
        System.out.println("Build expression in " + (currentTimeMillis() - t));
        t = currentTimeMillis();
        assertAnimoResult(e, "124750.");
        System.out.println("Eval expression in " + (currentTimeMillis() - t));
	}

	@Test
	public void test_01() throws Throwable {
        long t;
        t = currentTimeMillis();
        for (int i = 0; i < 500; i++) {
            __("def a" + i + " (a) (b '" + i + "').");
        }
        Expression e = new AnimoExpression("+ get b all a");
        System.out.println("Build expressions in " + (currentTimeMillis() - t));
        t = currentTimeMillis();
        assertAnimoResult(e, "124750.");
        System.out.println("Eval expression in " + (currentTimeMillis() - t));
	}

}