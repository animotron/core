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
 *  but WITHOUT ALL WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */
package org.animotron.statement.operator;

import org.animotron.ATest;
import org.junit.Ignore;
import org.junit.Test;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class AllAnyGetTest extends ATest {

    @Test
    public void test_000() throws Exception {
        testAnimo("the john sex male.");
        assertAnimoResult("all male", "the john sex.");
    }

    @Test
    public void test_010() throws Exception {
        testAnimo("the john sex male.");
        assertAnimoResult("any male", "the john sex.");
    }

    @Test
    public void test_020() throws Exception {
        testAnimo("the joe male.");
        assertAnimoResult("all male", "the joe male.");
    }

    @Test
    @Ignore //this test for USE, but it was't agreed yet
    public void test_030() throws Exception {
        testAnimo("the john (person) (sex male).");
        assertAnimoResult("any person use male", "the john (person) (sex).");
        assertAnimoResult("all person use male", "the john (person) (sex).");
        assertAnimoResult("prefer person use male", "the john (person) (sex).");
    }

    @Test
    public void test_031() throws Exception {
        testAnimo("the john person, male).");
        assertAnimoResult("any person use male", "the john person, male.");
        assertAnimoResult("all person use male", "the john person, male.");
        assertAnimoResult("prefer person use male", "the john person, male.");
    }

    @Test
    public void test_032() throws Exception {
        testAnimo("the john (person) (male).");
        assertAnimoResult("any person use male", "the john (person) (male).");
        assertAnimoResult("all person use male", "the john (person) (male).");
        assertAnimoResult("prefer person use male", "the john (person) (male).");
    }

    @Test
    public void test_040() throws Exception {
        testAnimo("the male sex.");
        testAnimo("the joe male.");
        testAnimo("the john sex male.");
        //assertAnimoResult("all male", "the joe male. the john sex.");
        assertAnimoResult("all male", "the joe male sex. the john sex.");
    }

    @Test
    public void test_050() throws Exception {
        testAnimo("the john sex male.");
        assertAnimoResult("get sex john", "sex male.");
    }

    @Test
    public void test_060() throws Exception {
        testAnimo("the male sex.");
        testAnimo("the joe male.");
        assertAnimoResult("get sex joe", "male.");
    }

    @Test
    public void test_070() throws Exception {
        testAnimo("the male sex.");
        testAnimo("the joe male.");
        testAnimo("the john sex male.");
        assertAnimoResult("get sex joe", "male.");
        assertAnimoResult("get sex john", "sex male sex.");
    }

}
