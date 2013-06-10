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
package org.animotron.statement.operator;

import org.animotron.ATest;
import org.animotron.expression.AnimoExpression;
import org.animotron.expression.Expression;
import org.junit.Ignore;
import org.junit.Test;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class ReferencesTest extends ATest {

    @Test
    public void test_000() throws Throwable {

        Expression A = new AnimoExpression("def A B C");
        assertAnimoResult(A, "A B.");

        __("def B C 'y'.");
        assertAnimoResult(A, "A B C.");
    }

    @Test
    @Ignore
    public void test_005() throws Throwable {
        testAnimo("def john sex male.");
        assertAnimoResult("all male", "john sex.");
    }

    @Test
    @Ignore
    public void test_010() throws Throwable {
        testAnimo("def john sex male.");
        assertAnimoResult("any male", "john sex.");
    }

    @Test
    public void test_020() throws Throwable {
        testAnimo("def joe male.");
        assertAnimoResult("all male", "joe male.");
    }

    @Test
    @Ignore //this test for USE, but it was't agreed yet
    public void test_030() throws Throwable {
        testAnimo("def john (person) (sex male).");
        assertAnimoResult("any person use male", "john (person) (sex).");
        assertAnimoResult("all person use male", "john (person) (sex).");
        assertAnimoResult("prefer person use male", "john (person) (sex).");
    }

    @Test
    public void test_031() throws Throwable {
        testAnimo("def john person, male.");
        assertAnimoResult("any person use male", "john (person) (male).");
        assertAnimoResult("all person use male", "john (person) (male).");
        assertAnimoResult("prefer person use male", "john (person) (male).");
    }

    @Test
    public void test_032() throws Throwable {
        testAnimo("def john (person) (male).");
        assertAnimoResult("any person use male", "john (person) (male).");
        assertAnimoResult("all person use male", "john (person) (male).");
        assertAnimoResult("prefer person use male", "john (person) (male).");
    }

    @Test
    @Ignore
    public void test_040() throws Throwable {
        testAnimo("def male sex.");
        testAnimo("def joe male.");
        testAnimo("def john sex male.");
        //assertAnimoResult("all male", "joe male. john sex.");
        assertAnimoResult("all male", "joe male sex. john sex.");
    }

    @Test
    public void test_050() throws Throwable {
        testAnimo("def john sex male.");
        assertAnimoResult("get sex john", "male.");
    }

    @Test
    public void test_060() throws Throwable {
        testAnimo("def male sex.");
        testAnimo("def joe male.");
        assertAnimoResult("get sex joe", "male sex.");
    }

    @Test
    public void test_070() throws Throwable {
        testAnimo("def male sex.");
        testAnimo("def joe male.");
        testAnimo("def john sex male.");
        assertAnimoResult("get sex joe", "male sex.");
        assertAnimoResult("get sex john", "male sex.");
    }
}