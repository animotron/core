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
import org.animotron.statement.relation.SHALL;
import org.animotron.statement.relation.USE;
import org.junit.Test;

import static org.animotron.expression.JExpression._;
import static org.animotron.expression.JExpression.value;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class ShallUseTest extends ATest {
	
	@Test
	public void test_0() throws Exception {
        
        JExpression.__(
                new JExpression(
                        _(THE._, "X", _(AN._, "A"))
                ),
                new JExpression(
                        _(THE._, "Y", _(AN._, "A"))
                ),
                new JExpression(
                        _(THE._, "B", _(SHALL._, "X", value("χ")), _(SHALL._, "Υ", value("υ")))
                ),
                new JExpression(
                        _(THE._, "C", _(AN._, "B"))
                )
        );

        JExpression D = new JExpression(
            _(THE._, "D", _(GET._, "A", _(AN._, "C", _(USE._, "X"))))
        );
        //XXX: assertAnimoResult(D, "the D X \"χ\".");
        assertAnimoResult(D, "the D shall X \"χ\".");
	}

    @Test
    public void test_1() throws Exception {

        JExpression.__(
                new JExpression(
                        _(THE._, "X", _(AN._, "A"))
                ),
                new JExpression(
                        _(THE._, "Y", _(AN._, "A"))
                ),
                new JExpression(
                        _(THE._, "B", _(SHALL._, "X", value("χ")), _(SHALL._, "Υ", value("υ")))
                ),
                new JExpression(
                        _(THE._, "C", _(AN._, "B"), _(USE._, "X"))
                )
        );

        JExpression D = new JExpression(
            _(THE._, "D", _(GET._, "A", _(AN._, "C")))
        );
        //XXX: assertAnimoResult(D, "the D X \"χ\".");
        assertAnimoResult(D, "the D shall X \"χ\".");
    }

    @Test
    public void test_2() throws Exception {

        JExpression.__(
                new JExpression(
                        _(THE._, "X", _(AN._, "A"))
                ),
                new JExpression(
                        _(THE._, "Y", _(AN._, "A"))
                ),
                new JExpression(
                        _(THE._, "B", _(SHALL._, "X", value("χ")), _(SHALL._, "Υ", value("υ")))
                ),
                new JExpression(
                        _(THE._, "C", _(AN._, "B"))
                ),
                new JExpression(
                        _(THE._, "U", _(USE._, "X"))
                )
        );
        JExpression D = new JExpression(
            _(THE._, "D", _(GET._, "A", _(AN._, "C", _(AN._, "U"))))
        );
        //XXX: assertAnimoResult(D, "the D X \"χ\".");
        assertAnimoResult(D, "the D shall X \"χ\".");
    }
}