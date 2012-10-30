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
    public void test_00() throws Throwable {
        testAnimo("def a x 1.");
        testAnimoResult("get x a.", "1.");
        eval(new AnimoExpression("change (// a x) (1) ((2) (3))."));
        //testAnimoResult("get x a.", "2.");
    }

    @Test
    public void test_01() throws Throwable {
        testAnimo("def a x y z any foo.");
        eval(new AnimoExpression("change (get y get x a) (any foo) (all bar)."));
    }

    @Test
    public void test_03() throws Throwable {
        testAnimo("def a (x) 1.");
        testAnimo("def b (x) 1.");
        testAnimoResult("all x.", "def a (x) 1. def b (x) 1.");
        eval(new AnimoExpression("change (all x) 1 2."));
    }

}