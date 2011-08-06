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
package org.animotron.operator;

import org.animotron.ATest;
import org.animotron.Expression;
import org.animotron.operator.query.ANY;
import org.animotron.operator.query.GET;
import org.animotron.operator.query.SELF;
import org.animotron.operator.relation.HAVE;
import org.animotron.operator.relation.IS;
import org.animotron.operator.relation.USE;
import org.junit.Test;

import static org.animotron.Expression._;
import static org.animotron.Expression.text;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class SelfUseTest extends ATest {
	
	@Test
	public void getFromPFlow() throws Exception {
        
    	new Expression(
			_(THE._, "A", _(IS._, "X"))
		);
    	
    	new Expression(
			_(THE._, "B", _(IS._, "X"))
		);
    	
    	Expression C = new Expression(
			_(THE._, "C", _(HAVE._, "A", text(".")), _(HAVE._, "B", _(SELF._, "X")))
		);

    	Expression CC = new Expression(
			_(THE._, "CC", _(HAVE._, "A", text("CC")), _(HAVE._, "B", _(SELF._, "X")))
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
    	
        assertAnimo(C, "<the:C><have:A>.</have:A><have:B><have:A>.</have:A></have:B></the:C>");
        assertAnimo(CC, "<the:CC><have:A>CC</have:A><have:B><have:A>CC</have:A></have:B></the:CC>");
        assertAnimo(D, "<the:D><is:C/><have:A>:</have:A></the:D>");
        assertAnimo(E, "<the:E><have:B><have:A>.</have:A></have:B></the:E>");
        assertAnimo(F, "<the:F><have:B><have:A>:</have:A></have:B></the:F>");

	}
	
}
