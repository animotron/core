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
import org.animotron.statement.query.GET;
import org.animotron.statement.query.SELF;
import org.junit.Ignore;
import org.junit.Test;

import static org.animotron.expression.JExpression._;
import static org.animotron.expression.JExpression.text;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class SelfTest extends ATest {
	
    @Test
    @Ignore
    public void getFromPFlow() throws Exception {
        
        JExpression.__(
                new JExpression(
                        _(THE._, "A")
                ),
                new JExpression(
                        _(THE._, "B")
                )
        );
    	
        JExpression C = new JExpression(
            _(THE._, "C", _(AN._, "A", text(".")), _(AN._, "B", _(SELF._, "A")))
        );

        JExpression CC = new JExpression(
            _(THE._, "CC", _(AN._, "A", text("CC")), _(AN._, "B", _(SELF._, "A")))
        );

        JExpression D = new JExpression(
            _(THE._, "D", _(AN._, "C"), _(AN._, "A", text(":")))
        );

        JExpression E = new JExpression(
            _(THE._, "E", _(GET._, "B", _(AN._, "C")))
        );
    	
        JExpression F = new JExpression(
            _(THE._, "F", _(GET._, "B", _(AN._, "D")))
        );
    	
        assertAnimoResult(C, "the C (A \".\") (B A \".\").");
        assertAnimoResult(CC, "the CC (A \"CC\") (B A \"CC\").");
        assertAnimoResult(D, "the D (C) (A \":\").");
        assertAnimoResult(E, "the E B A \".\".");
        assertAnimoResult(F, "the F B A \":\".");

    }
	
    @Test
    @Ignore
    public void getFromPFlow_by_IS() throws Exception {

        JExpression.__(
                new JExpression(
                        _(THE._, "A", _(AN._, "X"))
                ),
                new JExpression(
                        _(THE._, "B")
                )
        );

        JExpression C = new JExpression(
            _(THE._, "C", _(AN._, "A", text(".")), _(AN._, "B", _(SELF._, "X")))
        );
        assertAnimoResult(C, "the C (A \".\") (B A \".\").");

        JExpression CC = new JExpression(
            _(THE._, "CC", _(AN._, "A", text("CC")), _(AN._, "B", _(SELF._, "X")))
        );
        assertAnimoResult(CC, "the CC (A \"CC\") (B A \"CC\").");

        JExpression D = new JExpression(
            _(THE._, "D", _(AN._, "C"), _(AN._, "A", text(":")))
        );
        assertAnimoResult(D, "the D (C) (A \":\").");

        JExpression E = new JExpression(
            _(THE._, "E", _(GET._, "B", _(AN._, "C")))
        );
        assertAnimoResult(E, "the E B A \".\".");

        JExpression F = new JExpression(
            _(THE._, "F", _(GET._, "B", _(AN._, "D")))
        );
        assertAnimoResult(F, "the F B A \":\".");

        //second try to be sure
        assertAnimoResult(C, "the C (A \".\") (B A \".\").");
        assertAnimoResult(CC, "the CC (A \"CC\") (B A \"CC\").");
        assertAnimoResult(D, "the D (C) (A \":\").");
        assertAnimoResult(E, "the E B A \".\".");
        assertAnimoResult(F, "the F B A \":\".");

    }

}