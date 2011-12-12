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
import org.animotron.statement.compare.EQ;
import org.animotron.statement.compare.WITH;
import org.animotron.statement.query.ALL;
import org.junit.Test;

import static org.animotron.expression.JExpression._;
import static org.animotron.expression.JExpression.text;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class AllTest extends ATest {
	

    @Test
    public void testALL() throws Exception {

        JExpression.__(
            new JExpression(
                _(THE._, "A", _(AN._, "value"))
            ),
            new JExpression(
                _(THE._, "B", _(AN._, "A"), _(AN._, "value", text("B")))
            ),
            new JExpression(
                _(THE._, "C", _(AN._, "B"), _(AN._, "value", text("C")))
            )
        );

        JExpression test = new JExpression(
            _(ALL._, "A")
        );
        assertAnimoResultOneStep(test, "the C (B) (value \"C\").");

    }
	
    @Test
    public void testALLwithoutTHE() throws Exception {

        JExpression.__(
            new JExpression(
                _(THE._, "A", _(AN._, "value"))
            ),
            new JExpression(
                _(THE._, "B", _(AN._, "A"), _(AN._, "value", text("B")))
            ),
            new JExpression(
                _(THE._, "C", _(AN._, "B"), _(AN._, "value", text("C")))
            )
        );

        JExpression test = new JExpression(
            _(ALL._, "A")
        );
        assertAnimoResultOneStep(test, "the C (B) (value \"C\").");
        //assertAnimoResult(test, "the B\n    (A)\n    (value \"B\").\nthe C\n    (B)\n    (value \"C\").\n", true);
    }

    @Test
    public void testALLwithWITH() throws Exception {

        JExpression.__(
                new JExpression(
                        _(THE._, "A", _(AN._, "value"))
                ),
                new JExpression(
                        _(THE._, "B", _(AN._, "A"), _(AN._, "value", text("B")))
                ),
                new JExpression(
                        _(THE._, "B1", _(AN._, "B"), _(AN._, "value", text("B")))
                ),
                new JExpression(
                        _(THE._, "C", _(AN._, "B"), _(AN._, "value", text("C")))
                ),
                new JExpression(
                        _(THE._, "C1", _(AN._, "C"), _(AN._, "value", text("C")))
                )
        );

        JExpression test = new JExpression(
            _(ALL._, "A", _(WITH._, "value", text("B")))
        );
        assertAnimoResultOneStep(test, "the B1 (B) (value \"B\").");

        test = new JExpression(
            _(ALL._, "A", _(WITH._, "value", text("C")))
        );
        assertAnimoResultOneStep(test, "the C1 (C) (value \"C\").");
    }

	@Test
	public void ALLwithEQ() throws Exception {

        JExpression.__(
                new JExpression(
                        _(THE._, "text-plain",
                                _(AN._, "mime-type"),
                                _(AN._, "text"),
                                _(AN._, "type", text("text/plain")),
                                _(AN._, "reference", text("Plain text")),
                                _(AN._, "extension", text("txt"))
                        )
                ),
                new JExpression(
                        _(THE._, "application-atom",
                                _(AN._, "mime-type"),
                                _(AN._, "application"),
                                _(AN._, "type", text("application/atom+xml")),
                                _(AN._, "reference", text("Atom Feed Document")),
                                _(AN._, "extension", text("atom"))
                        )
                )
        );

		JExpression test = new JExpression(
			_(ALL._, "mime-type", _(EQ._, "extension", text("txt")))
		);
        //assertAnimoResult(test, "the test the text-plain (mime-type) (text) (type \"text/plain\") (reference \"Plain text\") (extension \"txt\").");
		assertAnimoResultOneStep(test, "the text-plain (mime-type) (text) (type \"text/plain\") (reference \"Plain text\") (extension \"txt\").");
	}
}
