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
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
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
import org.animotron.statement.query.ANY;
import org.junit.Test;

import static org.animotron.expression.JExpression._;
import static org.animotron.expression.JExpression.value;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class AnyTest extends ATest {
	
    @Test
    public void testANY() throws Exception {
        
        JExpression.__(
                new JExpression(
                        _(THE._, "A", _(AN._, "value"))
                ),
                new JExpression(
                        _(THE._, "B", _(AN._, "A"), _(AN._, "value", value("B")))
                ),
                new JExpression(
                        _(THE._, "C", _(AN._, "B"), _(AN._, "value", value("C")))
                )
        );

        JExpression test = new JExpression(
            _(ANY._, "A")
        );
        assertAnimoResultOneStep(test, "the B (A) (value \"B\").");

        test = new JExpression(
            _(ANY._, "B")
        );
        assertAnimoResultOneStep(test, "the C (B) (value \"C\").");
    }
	
    @Test
    public void testANYwithWITH() throws Exception {

        JExpression.__(
                new JExpression(
                        _(THE._, "A", _(AN._, "value"))
                ),
                new JExpression(
                        _(THE._, "B", _(AN._, "A"), _(AN._, "value", value("B")))
                ),
                new JExpression(
                        _(THE._, "B1", _(AN._, "B"), _(AN._, "value", value("B")))
                ),
                new JExpression(
                        _(THE._, "C", _(AN._, "B"), _(AN._, "value", value("C")))
                ),
                new JExpression(
                        _(THE._, "C1", _(AN._, "C"), _(AN._, "value", value("C")))
                )
        );

        JExpression test = new JExpression(
            _(ANY._, "A", _(WITH._, "value", value("B")))
        );
        assertAnimoResultOneStep(test, "the B (A) (value \"B\").");

        test = new JExpression(
            _(ANY._, "A", _(WITH._, "value", value("C")))
        );
        assertAnimoResultOneStep(test, "the C (B) (value \"C\").");
    }

	@Test
	public void ANYwithEQ() throws Exception {

		JExpression.__(
                new JExpression(
                        _(THE._, "value-plain",
                                _(AN._, "mime-type"),
                                _(AN._, "value"),
                                _(AN._, "type", value("value/plain")),
                                _(AN._, "reference", value("Plain value")),
                                _(AN._, "extension", value("txt"))
                        )
                ),
                new JExpression(
                        _(THE._, "application-atom",
                                _(AN._, "mime-type"),
                                _(AN._, "application"),
                                _(AN._, "type", value("application/atom+xml")),
                                _(AN._, "reference", value("Atom Feed Document")),
                                _(AN._, "extension", value("atom"))
                        )
                )
        );

		JExpression test = new JExpression(
			_(ANY._, "mime-type", _(EQ._, "extension", value("txt")))
		);
		assertAnimoResultOneStep(test, "the value-plain (mime-type) (value) (type \"value/plain\") (reference \"Plain value\") (extension \"txt\").");
	}
}