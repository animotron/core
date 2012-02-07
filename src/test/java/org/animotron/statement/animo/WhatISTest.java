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

}