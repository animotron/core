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
package org.animotron.statement.animo.update;

import org.animotron.ATest;
import org.animotron.expression.AnimoExpression;
import org.junit.Test;
import org.neo4j.graphdb.Relationship;

/**
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class UpdateTest extends ATest {

    @Test
    public void test_00() throws Throwable {
        Relationship a = testAnimo("def a x 1.");
        eval(new AnimoExpression("change (get x a) (1) ((2) (3))."));
        assertAnimo(a, "def a x 2 3.");
    }

    @Test
    public void test_01() throws Throwable {
        Relationship a = testAnimo("def a x y z any foo.");
        eval(new AnimoExpression("change (get y get x a) (any foo) (all bar)."));
        assertAnimo(a, "def a x y z all bar.");
    }

    @Test
    public void test_03() throws Throwable {
        Relationship a = testAnimo("def a (x) 1.");
        Relationship b = testAnimo("def b (x) 1.");
        eval(new AnimoExpression("change (all x) 1 2."));
        assertAnimo(a, "def a 2.");
        assertAnimo(b, "def b 2.");
    }

    @Test
    public void test_04() throws Throwable {
        Relationship a = testAnimo("def a (x) 1.");
        eval(new AnimoExpression("change (all x) 1 ((z) 2)."));
        assertAnimo(a, "def a (z) 2.");
    }

    @Test
    public void test_05() throws Throwable {
        Relationship a = testAnimo("def a 1.");
        eval(new AnimoExpression("change a 1 2."));
        assertAnimo(a, "def a 2.");
    }

    @Test
    public void test_06() throws Throwable {
        Relationship a = testAnimo("def a 1.");
        eval(new AnimoExpression("change a 2."));
        assertAnimo(a, "def a 2.");
    }

    @Test
    public void test_07() throws Throwable {
        Relationship a = testAnimo("def a 1.");
        eval(new AnimoExpression("change a 2."));
        assertAnimo(a, "def a 2.");
        eval(new AnimoExpression("change a 3."));
        assertAnimo(a, "def a 3.");
    }

    @Test
    public void test_08() throws Throwable {
        Relationship a = testAnimo("def a.");
        eval(new AnimoExpression("change a 1."));
        assertAnimo(a, "def a 1.");
    }

    @Test
    public void test_09() throws Throwable {
        Relationship a = testAnimo("def a.");
        Relationship b = testAnimo("def b.");
        eval(new AnimoExpression("change a,b 1."));
        assertAnimo(a, "def a 1.");
        assertAnimo(b, "def b 1.");
    }

}