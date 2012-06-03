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
package org.animotron.statement.operator;

import static org.animotron.expression.JExpression._;
import static org.animotron.expression.JExpression.value;

import org.animotron.ATest;
import org.animotron.expression.JExpression;
import org.junit.Ignore;
import org.junit.Test;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class ReferencesTest extends ATest {

    @Test
    public void test() throws Throwable {

        JExpression A = new JExpression(
            _(DEF._, "A", _(AN._, "B", _(AN._, "C")))
        );
        //assertAnimoResultOneStep(A, "def A the B.");
        assertAnimoResultOneStep(A, "def A B.");

        JExpression.__(new JExpression(
            _(DEF._, "B", _(AN._, "C", value("y")))
        ));
        assertAnimoResultOneStep(A, "def A B C \"y\".");
    }

    @Test
    public void test_000() throws Throwable {
        testAnimo("def john sex male.");
        assertAnimoResult("all male", "def john sex.");
    }

    @Test
    public void test_010() throws Throwable {
        testAnimo("def john sex male.");
        assertAnimoResult("any male", "def john sex.");
    }

    @Test
    public void test_020() throws Throwable {
        testAnimo("def joe male.");
        assertAnimoResult("all male", "def joe male.");
    }

    @Test
    @Ignore //this test for USE, but it was't agreed yet
    public void test_030() throws Throwable {
        testAnimo("def john (person) (sex male).");
        assertAnimoResult("any person use male", "def john (person) (sex).");
        assertAnimoResult("all person use male", "def john (person) (sex).");
        assertAnimoResult("prefer person use male", "def john (person) (sex).");
    }

    @Test
    public void test_031() throws Throwable {
        testAnimo("def john person, male.");
        assertAnimoResult("any person use male", "def john (person) (male).");
        assertAnimoResult("all person use male", "def john (person) (male).");
        assertAnimoResult("prefer person use male", "def john (person) (male).");
    }

    @Test
    public void test_032() throws Throwable {
        testAnimo("def john (person) (male).");
        assertAnimoResult("any person use male", "def john (person) (male).");
        assertAnimoResult("all person use male", "def john (person) (male).");
        assertAnimoResult("prefer person use male", "def john (person) (male).");
    }

    @Test
    public void test_040() throws Throwable {
        testAnimo("def male sex.");
        testAnimo("def joe male.");
        testAnimo("def john sex male.");
        //assertAnimoResult("all male", "def joe male. the john sex.");
        assertAnimoResult("all male", "def joe male sex. the john sex.");
    }

    @Test
    public void test_050() throws Throwable {
        testAnimo("def john sex male.");
        assertAnimoResult("get sex john", "sex male.");
    }

    @Test
    public void test_060() throws Throwable {
        testAnimo("def male sex.");
        testAnimo("def joe male.");
        assertAnimoResult("get sex joe", "male.");
    }

    @Test
    public void test_070() throws Throwable {
        testAnimo("def male sex.");
        testAnimo("def joe male.");
        testAnimo("def john sex male.");
        assertAnimoResult("get sex joe", "male.");
        assertAnimoResult("get sex john", "sex male sex.");
    }

	@Test
    public void test_100() throws Throwable {
        testAnimo("def xxx-site (site) (name \"XXX\").");
        testAnimo("def xxx-layout (layout) (get name).");
        testAnimo("def html-page any layout.");
        testAnimo("def xxx (any site) (html-page).");
        assertStringResult("xxx", "XXX");
    }
}