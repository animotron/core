package org.animotron.statement.animo;

import org.animotron.ATest;
import org.junit.Test;

public class WhatISTest extends ATest {

	@Test
	public void test_00() throws Throwable {
        testAnimo("the a x.");
        
        assertAnimoResult("what-is a", "x.");
	}

    @Test
    public void test_01() throws Throwable {
        testAnimo("the a (x) (y).");

        assertAnimoResult("what-is a", "x. y.");
    }

//    @Test
//    public void test_02() throws IOException {
//        testAnimo("the a (x) (y) (z).");
//
//        assertAnimoResult("what-is a use y", "y.");
//    }

    @Test
    public void test_03() throws Throwable {
        animo("the a x.", "the b y.", "the c b a.");

        assertAnimoResult("what-is get b c", "y.");

        assertAnimoResult("each (get b c) (what-is this b)", "x. y.");
    }

    @Test
    public void test_04() throws Throwable {
        animo("the a x.", "the b y.", "the c part (a) (b).");

        assertAnimoResult("each (get part c) (what-is this part)", "x. y.");
    }

    @Test
    public void test_05() throws Throwable {
        animo("the a x.", "the b y.", "the c part (a) (b).", "the part foo.");

        assertAnimoResult("each (get part c) (what-is this part)", "x. foo. y. foo.");
    }

    @Test
    public void test_06() throws Throwable {
        animo("the foo bar.", "the a bar x.");

        assertAnimoResult("get (what-is foo) (a)", "bar x.");
    }

}