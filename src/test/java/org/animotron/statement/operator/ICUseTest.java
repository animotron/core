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
import org.animotron.statement.query.GET;
import org.animotron.statement.relation.IS;
import org.animotron.statement.relation.USE;
import org.junit.Test;

import static org.animotron.expression.JExpression._;
import static org.animotron.expression.JExpression.text;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class ICUseTest extends ATest {
	
	@Test
	public void testIC() throws Exception {
        
        JExpression.__(
                new JExpression(
                        _(THE._, "X", _(IS._, "A"))
                ),
                new JExpression(
                        _(THE._, "Y", _(IS._, "A"))
                ),
                new JExpression(
                        _(THE._, "B", _(IC._, "X", text("χ")), _(IC._, "Υ", text("υ")))
                ),
                new JExpression(
                        _(THE._, "C", _(IS._, "B"))
                )
        );

        JExpression D = new JExpression(
            _(THE._, "D", _(GET._, "A", _(AN._, "C", _(USE._, "X"))))
        );
        assertAnimoResult(D, "the D have X \"χ\".");

	}

    @Test
    public void testIC_1() throws Exception {

        JExpression.__(
                new JExpression(
                        _(THE._, "X", _(IS._, "A"))
                ),
                new JExpression(
                        _(THE._, "Y", _(IS._, "A"))
                ),
                new JExpression(
                        _(THE._, "B", _(IC._, "X", text("χ")), _(IC._, "Υ", text("υ")))
                ),
                new JExpression(
                        _(THE._, "C", _(IS._, "B"), _(USE._, "X"))
                )
        );

        JExpression D = new JExpression(
            _(THE._, "D", _(GET._, "A", _(AN._, "C")))
        );
        assertAnimoResult(D, "the D have X \"χ\".");

    }

    @Test
    public void testIC_2() throws Exception {

        JExpression.__(
                new JExpression(
                        _(THE._, "X", _(IS._, "A"))
                ),
                new JExpression(
                        _(THE._, "Y", _(IS._, "A"))
                ),
                new JExpression(
                        _(THE._, "B", _(IC._, "X", text("χ")), _(IC._, "Υ", text("υ")))
                ),
                new JExpression(
                        _(THE._, "C", _(IS._, "B"))
                ),
                new JExpression(
                        _(THE._, "U", _(USE._, "X"))
                )
        );
        JExpression D = new JExpression(
            _(THE._, "D", _(GET._, "A", _(AN._, "C", _(AN._, "U"))))
        );
        assertAnimoResult(D, "the D have X \"χ\".");

    }

}