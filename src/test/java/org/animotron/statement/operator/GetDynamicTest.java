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
import org.animotron.expression.JExpression;
import org.animotron.statement.query.ALL;
import org.animotron.statement.query.GET;
import org.junit.Test;

import static org.animotron.expression.JExpression._;
import static org.animotron.expression.JExpression.text;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class GetDynamicTest extends ATest {
	
	@Test
	public void getOnManyAN() throws Exception {

        JExpression.__(
            new JExpression(
                _(THE._, "A", _(AN._, "Z", text("A")))
            ),
            new JExpression(
                _(THE._, "B", _(AN._, "Z", text("B")))
            )
        );

    	JExpression d = new JExpression(
			_(THE._, "d", _(GET._, _(AN._, "Z"), _(AN._, "A"), _(AN._, "B")))
		);
        assertAnimoResult(d, "the d (Z \"A\") (Z \"B\").");
	}

	@Test
	public void getOnManyANbyIS() throws Exception {

        JExpression.__(
            new JExpression(
                _(THE._, "ZZ", _(AN._, "Z"))
            ),
            new JExpression(
                _(THE._, "A", _(AN._, "Z", text("A")))
            ),
            new JExpression(
                _(THE._, "B", _(AN._, "ZZ", text("B")))
            )
        );

    	JExpression d = new JExpression(
			_(THE._, "d", _(GET._, _(AN._, "Z"), _(AN._, "A"), _(AN._, "B")))
		);
        assertAnimoResult(d, "the d (Z \"A\") (ZZ \"B\").");
	}

	@Test
    public void get_via_is() throws Exception {

        JExpression.__(
            new JExpression(
                _(THE._, "B", _(AN._, "A"))
            ),
            new JExpression(
                _(THE._, "C", _(AN._, "Z"), _(AN._, "B", text("π")))
            ),
            new JExpression(
                _(THE._, "D", _(AN._, "Z"), _(AN._, "A", text("Aπ")))
            )
        );

        JExpression E = new JExpression(
            _(THE._, "E", _(GET._, "A", _(ALL._, "Z")))
        );
        assertAnimoResult(E, "the E (B \"π\") (A \"Aπ\").");

        JExpression E1 = new JExpression(
            _(THE._, "E1", _(GET._, "B", _(ALL._, "Z")))
        );
        assertAnimoResult(E1, "the E1 B \"π\".");

        JExpression F = new JExpression(
            _(THE._, "F", _(GET._, _(ALL._, "A"), _(ALL._, "Z")))
        );
        assertAnimoResult(F, "the F B \"π\".");
    }

    @Test
    public void test_00() throws Exception {
    	testAnimo("the z a z1.");
    	testAnimo("the b z1 \"z1\".");

    	testAnimoResult("get (get a z) (b).", "z1 \"z1\".");
    }

    @Test
    public void test_01() throws Exception {
    	testAnimo("the z (k) (a z1).");
    	testAnimo("the b (k) (z1 \"z1\").");

    	testAnimoResult("get (get a z) (all k).", "a z1. z1 \"z1\".");
    }

    @Test
    public void test_02() throws Exception {
    	testAnimo("the z (k) (a z1).");
    	testAnimo("the b (k) (z1 \"z1\").");

    	testAnimoResult("get (get a z) (a).", "");
    }

    @Test
    public void test_03() throws Exception {
    	testAnimo("the z (k) (a z1).");
    	testAnimo("the b (z) (z1 \"z1\").");
    	testAnimo("the c (z) (z1 \"2z1\").");

    	testAnimoResult("get (get a z) (all z).", "z1 \"z1\". z1 \"2z1\".");
    }
}