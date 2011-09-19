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
import org.animotron.statement.query.GET;
import org.animotron.statement.query.SELF;
import org.animotron.statement.relation.HAVE;
import org.animotron.statement.relation.IS;
import org.junit.Test;

import static org.animotron.Expression._;
import static org.animotron.Expression.text;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class SelfTest extends ATest {
	
    @Test
    public void getFromPFlow() throws Exception {
        
        new Expression(
            _(THE._, "A")
        );
    	
        new Expression(
            _(THE._, "B")
        );
    	
        Expression C = new Expression(
            _(THE._, "C", _(HAVE._, "A", text(".")), _(HAVE._, "B", _(SELF._, "A")))
        );

        Expression CC = new Expression(
            _(THE._, "CC", _(HAVE._, "A", text("CC")), _(HAVE._, "B", _(SELF._, "A")))
        );

        Expression D = new Expression(
            _(THE._, "D", _(IS._, "C"), _(HAVE._, "A", text(":")))
        );

        Expression E = new Expression(
            _(THE._, "E", _(GET._, "B", _(AN._, "C")))
        );
    	
        Expression F = new Expression(
            _(THE._, "F", _(GET._, "B", _(AN._, "D")))
        );
    	
        assertAnimoResult(C, "the C (have A \".\") (have B have A \".\")");
        assertAnimoResult(CC, "the CC (have A \"CC\") (have B have A \"CC\")");
        assertAnimoResult(D, "the D (is C) (have A \":\")");
        assertAnimoResult(E, "the E have B have A \".\"");
        assertAnimoResult(F, "the F have B have A \":\"");
    }
	
    @Test
    public void getFromPFlow_by_IS() throws Exception {

        new Expression(
            _(THE._, "A", _(IS._, "X"))
        );

        new Expression(
            _(THE._, "B")
        );

        Expression C = new Expression(
            _(THE._, "C", _(HAVE._, "A", text(".")), _(HAVE._, "B", _(SELF._, "X")))
        );
        assertAnimoResult(C, "the C (have A \".\") (have B have A \".\")");

        Expression CC = new Expression(
            _(THE._, "CC", _(HAVE._, "A", text("CC")), _(HAVE._, "B", _(SELF._, "X")))
        );
        assertAnimoResult(CC, "the CC (have A \"CC\") (have B have A \"CC\")");

        Expression D = new Expression(
            _(THE._, "D", _(IS._, "C"), _(HAVE._, "A", text(":")))
        );
        assertAnimoResult(D, "the D (is C) (have A \":\")");

        Expression E = new Expression(
            _(THE._, "E", _(GET._, "B", _(AN._, "C")))
        );
        assertAnimoResult(E, "the E have B have A \".\"");

        Expression F = new Expression(
            _(THE._, "F", _(GET._, "B", _(AN._, "D")))
        );
        assertAnimoResult(F, "the F have B have A \":\"");

        //second try to be sure
        assertAnimoResult(C, "the C (have A \".\") (have B have A \".\")");
        assertAnimoResult(CC, "the CC (have A \"CC\") (have B have A \"CC\")");
        assertAnimoResult(D, "the D (is C) (have A \":\")");
        assertAnimoResult(E, "the E have B have A \".\"");
        assertAnimoResult(F, "the F have B have A \":\"");
    }
}