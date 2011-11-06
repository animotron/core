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
import org.animotron.statement.query.SELF;
import org.animotron.statement.relation.HAVE;
import org.animotron.statement.relation.IS;
import org.animotron.statement.relation.USE;
import org.junit.Test;

import static org.animotron.expression.JExpression._;
import static org.animotron.expression.JExpression.text;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class SelfUseTest extends ATest {
	
    @Test
    public void selfTest() throws Exception {
        
        JExpression.__(
                new JExpression(
                        _(THE._, "A", _(IS._, "X"))
                ),
                new JExpression(
                        _(THE._, "B", _(IS._, "X"))
                ),
                new JExpression(
                        _(THE._, "C", _(HAVE._, "A", text(".")), _(HAVE._, "B", text("..")))
                ),
                new JExpression(
                        _(THE._, "D", _(IS._, "C"), _(SELF._, "X"))
                )
        );

        JExpression a = new JExpression(
            _(THE._, "a", _(AN._, "D", _(USE._, "A")))
        );
        assertAnimoResult(a, "the a the D (is C) (have A \".\").");

        JExpression b = new JExpression(
            _(THE._, "b", _(AN._, "D", _(USE._, "B")))
        );
        assertAnimoResult(b, "the b the D (is C) (have B \"..\").");

    }
	
    @Test
    public void selfTest1() throws Exception {

        JExpression.__(
                new JExpression(
                        _(THE._, "A", _(IS._, "X"))
                ),
                new JExpression(
                        _(THE._, "B", _(IS._, "X"))
                ),
                new JExpression(
                        _(THE._, "C", _(HAVE._, "A", text(".")), _(HAVE._, "B", text("..")))
                ),
                new JExpression(
                        _(THE._, "D", _(IS._, "C"), _(SELF._, "X"))
                ),
                new JExpression(
                        _(THE._, "ua", _(USE._, "A"))
                ),
                new JExpression(
                        _(THE._, "ub", _(USE._, "B"))
                )
        );

        JExpression a = new JExpression(
            _(THE._, "a", _(AN._, "D", _(AN._, "ua")))
        );
        assertAnimoResult(a, "the a the D (is C) (have A \".\").");

        JExpression b = new JExpression(
            _(THE._, "b", _(AN._, "D", _(AN._, "ub")))
        );
        assertAnimoResult(b, "the b the D (is C) (have B \"..\").");

    }

}