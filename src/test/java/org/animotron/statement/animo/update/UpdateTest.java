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
import org.junit.Ignore;
import org.junit.Test;
import org.neo4j.graphdb.Relationship;

import static org.animotron.expression.Expression.__;

/**
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class UpdateTest extends ATest {

    @Test
    @Ignore
	public void test_00() throws Throwable {
        testAnimo("def a x 1.");
        eval(new AnimoExpression("add (get x a) (y 2)."));
        //assertAnimo(e, "def a x 1 (y 2).");
        eval(new AnimoExpression("add a z 3."));
        //assertAnimo(e, "def a (x 1 (y 2)) (z 3).");
        eval(new AnimoExpression("delete get x a."));
        //assertAnimo(e, "def a y 2.");
	}

    @Test
    @Ignore
    public void test_001() throws Throwable {
        __(new AnimoExpression("def a x z."));
        assertAnimoResult("all x", "z.");
    }

    @Test
    @Ignore
    public void test_002() throws Throwable {
        __(new AnimoExpression("def a x 1."));
        assertAnimoResult(new AnimoExpression("all x"), "1.");
    }

    @Test
    @Ignore
	public void test_01() throws Throwable {
        Relationship e = __(new AnimoExpression("def a x 1."));
        eval(new AnimoExpression("replace (get x a) (y 2)."));
        assertAnimo(e, "def a y 2.");
	}

    @Test
    @Ignore
	public void test_02() throws Throwable {
        Relationship e = __(new AnimoExpression("def a x 1."));
        eval(new AnimoExpression("set (get x a) (2)."));
        assertAnimo(e, "def a x 2.");
	}

    @Test
    @Ignore
	public void test_03() throws Throwable {
        Relationship[] e = __(
            new AnimoExpression("def a x 1."),
            new AnimoExpression("def b x 1."),
            new AnimoExpression("def c x 1.")
        );
        eval(new AnimoExpression("set (get x a) (2)."));
        assertAnimo(e[0], "def a x 2.");
        assertAnimo(e[1], "def b x 1.");
        assertAnimo(e[2], "def c x 1.");
	}

    @Test
    @Ignore
	public void test_04() throws Throwable {
        Relationship[] e = __(
            new AnimoExpression("def a x 1."),
            new AnimoExpression("def b x 1."),
            new AnimoExpression("def c x 1.")
        );
        eval(new AnimoExpression("replace (get x a) (y 2)."));
        assertAnimo(e[0], "def a y 2.");
        assertAnimo(e[1], "def b x 1.");
        assertAnimo(e[2], "def c x 1.");
	}

    @Test
    @Ignore
	public void test_05() throws Throwable {
        Relationship[] e = __(
            new AnimoExpression("def a z y x 1."),
            new AnimoExpression("def b x 1."),
            new AnimoExpression("def c x 1.")
        );
        eval(new AnimoExpression("set (get z get y get x a) (2)."));
        assertAnimo(e[0], "def a z y x 2.");
        assertAnimo(e[1], "def b x 1.");
        assertAnimo(e[2], "def c x 1.");
	}

    @Test
    @Ignore
	public void test_06() throws Throwable {
        Relationship[] e = __(
            new AnimoExpression("def a z y x 1."),
            new AnimoExpression("def b x 1."),
            new AnimoExpression("def c x 1.")
        );
        eval(new AnimoExpression("replace (get z get y get x a) (y 2)."));
        assertAnimo(e[0], "def a z y y 2.");
        assertAnimo(e[1], "def b x 1.");
        assertAnimo(e[2], "def c x 1.");
	}

    @Test
    @Ignore
	public void test_07() throws Throwable {
        Relationship[] e = __(
            new AnimoExpression("def a z y x 1."),
            new AnimoExpression("def b z y x 1."),
            new AnimoExpression("def c z y x 1.")
        );
        eval(new AnimoExpression("set (get z get y get x a) (2)."));
        assertAnimo(e[0], "def a z y x 2.");
        assertAnimo(e[1], "def b z y x 1.");
        assertAnimo(e[2], "def c z y x 1.");
	}

    @Test
    @Ignore
	public void test_08() throws Throwable {
        Relationship[] e = __(
            new AnimoExpression("def a z y x 1."),
            new AnimoExpression("def b z y x 1."),
            new AnimoExpression("def c z y x 1.")
        );
        eval(new AnimoExpression("replace (get z get y get x a) (y 2)."));
        assertAnimo(e[0], "def a z y y 2.");
        assertAnimo(e[1], "def b z y x 1.");
        assertAnimo(e[2], "def c z y x 1.");
	}

    @Test
    @Ignore
	public void test_09() throws Throwable {
        Relationship[] e = __(
            new AnimoExpression("def a z (y x 1) (α 3)."),
            new AnimoExpression("def b z (y x 1) (α 3)."),
            new AnimoExpression("def c z y x 1.")
        );
        eval(new AnimoExpression("set (get z get y get x a) (2)."));
        assertAnimo(e[0], "def a z (y x 2) (α 3).");
        assertAnimo(e[1], "def b z (y x 1) (α 3).");
        assertAnimo(e[2], "def c z y x 1.");
	}

    @Test
    @Ignore
	public void test_10() throws Throwable {
        Relationship[] e = __(
            new AnimoExpression("def a z (y x 1) (α 3)."),
            new AnimoExpression("def b z (y x 1) (α 3)."),
            new AnimoExpression("def c z y x 1.")
        );
        eval(new AnimoExpression("replace (get z get y get x a) (y 2)."));
        assertAnimo(e[0], "def a z (y y 2) (α 3).");
        assertAnimo(e[1], "def b z (y x 1) (α 3).");
        assertAnimo(e[2], "def c z y x 1.");
	}
}