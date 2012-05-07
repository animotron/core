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

import org.animotron.ATest;
import org.animotron.expression.JExpression;
import org.junit.Ignore;
import org.junit.Test;

import static org.animotron.expression.JExpression._;
import static org.animotron.expression.JExpression.value;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class ReferencesTest extends ATest {

    @Test
    public void test() throws Throwable {

        JExpression A = new JExpression(
            _(THE._, "A", _(AN._, "B", _(AN._, "C")))
        );
        //assertAnimoResultOneStep(A, "the A the B.");
        assertAnimoResultOneStep(A, "the A B.");

        new JExpression(
            _(THE._, "B", _(AN._, "C", value("y")))
        );
        assertAnimoResultOneStep(A, "the A B C \"y\".");
    }

    @Test
    public void test_000() throws Throwable {
        testAnimo("the john sex male.");
        assertAnimoResult("all male", "the john sex.");
    }

    @Test
    public void test_010() throws Throwable {
        testAnimo("the john sex male.");
        assertAnimoResult("any male", "the john sex.");
    }

    @Test
    public void test_020() throws Throwable {
        testAnimo("the joe male.");
        assertAnimoResult("all male", "the joe male.");
    }

    @Test
    @Ignore //this test for USE, but it was't agreed yet
    public void test_030() throws Throwable {
        testAnimo("the john (person) (sex male).");
        assertAnimoResult("any person use male", "the john (person) (sex).");
        assertAnimoResult("all person use male", "the john (person) (sex).");
        assertAnimoResult("prefer person use male", "the john (person) (sex).");
    }

    @Test
    public void test_031() throws Throwable {
        testAnimo("the john person, male.");
        assertAnimoResult("any person use male", "the john (person) (male).");
        assertAnimoResult("all person use male", "the john (person) (male).");
        assertAnimoResult("prefer person use male", "the john (person) (male).");
    }

    @Test
    public void test_032() throws Throwable {
        testAnimo("the john (person) (male).");
        assertAnimoResult("any person use male", "the john (person) (male).");
        assertAnimoResult("all person use male", "the john (person) (male).");
        assertAnimoResult("prefer person use male", "the john (person) (male).");
    }

    @Test
    public void test_040() throws Throwable {
        testAnimo("the male sex.");
        testAnimo("the joe male.");
        testAnimo("the john sex male.");
        //assertAnimoResult("all male", "the joe male. the john sex.");
        assertAnimoResult("all male", "the joe male sex. the john sex.");
    }

    @Test
    public void test_050() throws Throwable {
        testAnimo("the john sex male.");
        assertAnimoResult("get sex john", "sex male.");
    }

    @Test
    public void test_060() throws Throwable {
        testAnimo("the male sex.");
        testAnimo("the joe male.");
        assertAnimoResult("get sex joe", "male.");
    }

    @Test
    public void test_070() throws Throwable {
        testAnimo("the male sex.");
        testAnimo("the joe male.");
        testAnimo("the john sex male.");
        assertAnimoResult("get sex joe", "male.");
        assertAnimoResult("get sex john", "sex male sex.");
    }

	@Test
    public void test_100() throws Throwable {
        testAnimo("the xxx-site (site) (name \"XXX\").");
        testAnimo("the xxx-layout (layout) (get name).");
        testAnimo("the html-page any layout.");
        testAnimo("the xxx (any site) (html-page).");
        assertStringResult("xxx", "XXX");
    }
}