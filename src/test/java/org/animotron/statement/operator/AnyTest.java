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
import org.animotron.statement.relation.HAVE;
import org.animotron.statement.relation.IS;
import org.junit.Test;

import static org.animotron.expression.JExpression._;
import static org.animotron.expression.JExpression.text;

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
                        _(THE._, "A", _(HAVE._, "value"))
                ),
                new JExpression(
                        _(THE._, "B", _(IS._, "A"), _(HAVE._, "value", text("B")))
                ),
                new JExpression(
                        _(THE._, "C", _(IS._, "B"), _(HAVE._, "value", text("C")))
                )
        );

        JExpression a = new JExpression(
            _(THE._, "a", _(ANY._, "A"))
        );
        assertAnimoResult(a, "the a the B (is A) (have value \"B\")");

        JExpression b = new JExpression(
            _(THE._, "b", _(ANY._, "B"))
        );
        assertAnimoResult(b, "the b the C (is B) (have value \"C\")");

    }
	
    @Test
    public void testANYwithWITH() throws Exception {

        JExpression.__(
                new JExpression(
                        _(THE._, "A", _(HAVE._, "value"))
                ),
                new JExpression(
                        _(THE._, "B", _(IS._, "A"), _(HAVE._, "value", text("B")))
                ),
                new JExpression(
                        _(THE._, "B1", _(IS._, "B"), _(HAVE._, "value", text("B")))
                ),
                new JExpression(
                        _(THE._, "C", _(IS._, "B"), _(HAVE._, "value", text("C")))
                ),
                new JExpression(
                        _(THE._, "C1", _(IS._, "C"), _(HAVE._, "value", text("C")))
                )
        );

        JExpression D = new JExpression(
            _(THE._, "D", _(ANY._, "A", _(WITH._, "value", text("B"))))
        );
        assertAnimoResult(D, "the D the B (is A) (have value \"B\")");

        JExpression E = new JExpression(
            _(THE._, "E", _(ANY._, "A", _(WITH._, "value", text("C"))))
        );
        assertAnimoResult(E, "the E the C (is B) (have value \"C\")");

    }

	@Test
	public void ANYwithEQ() throws Exception {

		JExpression.__(
                new JExpression(
                        _(THE._, "text-plain",
                                _(IS._, "mime-type"),
                                _(IS._, "text"),
                                _(HAVE._, "type", text("text/plain")),
                                _(HAVE._, "reference", text("Plain text")),
                                _(HAVE._, "extension", text("txt"))
                        )
                ),
                new JExpression(
                        _(THE._, "application-atom",
                                _(IS._, "mime-type"),
                                _(IS._, "application"),
                                _(HAVE._, "type", text("application/atom+xml")),
                                _(HAVE._, "reference", text("Atom Feed Document")),
                                _(HAVE._, "extension", text("atom"))
                        )
                )
        );

		JExpression test = new JExpression(
			_(THE._, "test", 
				_(ANY._, "mime-type", _(EQ._, "extension", text("txt")))
			)
		);
        assertAnimoResult(test, "the test the text-plain (is mime-type) (is text) (have type \"text/plain\") (have reference \"Plain text\") (have extension \"txt\")");

	}

}