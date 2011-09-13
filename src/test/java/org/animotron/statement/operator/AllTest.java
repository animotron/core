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
import org.animotron.Expression;
import org.animotron.statement.compare.EQ;
import org.animotron.statement.compare.WITH;
import org.animotron.statement.query.ALL;
import org.animotron.statement.relation.HAVE;
import org.animotron.statement.relation.IS;
import org.junit.Test;

import static org.animotron.Expression._;
import static org.animotron.Expression.text;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class AllTest extends ATest {
	

    @Test
    public void testALL() throws Exception {
        
        new Expression(
            _(THE._, "A", _(HAVE._, "value"))
        );
	
        new Expression(
            _(THE._, "B", _(IS._, "A"), _(HAVE._, "value", text("B")))
        );

        new Expression(
            _(THE._, "C", _(IS._, "B"), _(HAVE._, "value", text("C")))
        );

        Expression D = new Expression(
            _(THE._, "D", _(ALL._, "A"))
        );
        assertAnimoResult(D, "the D (the B (is A) (have value \"B\")) (the C (is B) (have value \"C\"))");
    }
	
    @Test
    public void testALLwithWITH() throws Exception {

        new Expression(
            _(THE._, "A", _(HAVE._, "value"))
        );

        new Expression(
            _(THE._, "B", _(IS._, "A"), _(HAVE._, "value", text("B")))
        );

        new Expression(
            _(THE._, "B1", _(IS._, "B"), _(HAVE._, "value", text("B")))
        );

        new Expression(
            _(THE._, "C", _(IS._, "B"), _(HAVE._, "value", text("C")))
        );
        new Expression(
            _(THE._, "C1", _(IS._, "C"), _(HAVE._, "value", text("C")))
        );

        Expression D = new Expression(
            _(THE._, "D", _(ALL._, "A", _(WITH._, "value", text("B"))))
        );
        assertAnimoResult(D, "the D (the B (is A) (have value \"B\")) (the B1 (is B) (have value \"B\"))");

        Expression E = new Expression(
            _(THE._, "E", _(ALL._, "A", _(WITH._, "value", text("C"))))
        );
        assertAnimoResult(E, "the E (the C (is B) (have value \"C\")) (the C1 (is C) (have value \"C\"))");
    }

	@Test
	public void ALLwithEQ() throws Exception {

		new Expression(
			_(THE._, "text-plain", 
				_(IS._, "mime-type"),
				_(IS._, "text"),
				_(HAVE._, "type", text("text/plain")),
				_(HAVE._, "reference", text("Plain text")),
				_(HAVE._, "extension", text("txt"))
				)
		);
    	
		new Expression(
				_(THE._, "application-atom", 
					_(IS._, "mime-type"),
					_(IS._, "application"),
					_(HAVE._, "type", text("application/atom+xml")),
					_(HAVE._, "reference", text("Atom Feed Document")),
					_(HAVE._, "extension", text("atom"))
					)
			);

		Expression test = new Expression(
			_(THE._, "test", 
				_(ALL._, "mime-type", _(EQ._, "extension", text("txt")))
			)
		);
	
        assertAnimoResult(test, "the test the text-plain (is mime-type) (is text) (have type \"text/plain\") (have reference \"Plain text\") (have extension \"txt\")");
	}
}
