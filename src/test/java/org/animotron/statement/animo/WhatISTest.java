package org.animotron.statement.animo;

import static org.animotron.expression.AnimoExpression.__;

import java.io.IOException;

import org.animotron.ATest;
import org.junit.Test;

public class WhatISTest extends ATest {

	@Test
	public void test_00() throws IOException {
        __("the a x.");
        
        assertAnimoResult("what-is a", "x.");
	}
}