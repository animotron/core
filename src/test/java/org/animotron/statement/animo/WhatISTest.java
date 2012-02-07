package org.animotron.statement.animo;

import org.animotron.ATest;
import org.junit.Test;

import java.io.IOException;

import static org.animotron.expression.AnimoExpression.__;

public class WhatISTest extends ATest {

	@Test
	public void test_00() throws IOException {
        __("the a x.");
        
        assertAnimoResult("what-is a", "x.");
	}

    @Test
    public void test_01() throws IOException {
        __("the a (x) (y).");

        assertAnimoResult("what-is a", "x. y.");
    }

    @Test
    public void test_02() throws IOException {
        __("the a (x) (y) (z).");

        assertAnimoResult("what-is a use y", "y.");
    }

    @Test
    public void test_03() throws IOException {
        __("the a x.", "the b y.", "the c b a.");

        assertAnimoResult("what-is get b c", "y.");
    }

    @Test
    public void test_04() throws IOException {
        __("the a x.", "the b y.", "the c part (a) (b).");

        assertAnimoResult("each (get part c) (what-is this part)", "x. y.");
    }

}