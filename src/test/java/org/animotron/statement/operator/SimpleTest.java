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
import org.animotron.Expression;
import org.animotron.statement.operator.query.GET;
import org.animotron.statement.relation.HAVE;
import org.junit.Test;

import static org.animotron.Expression._;
import static org.animotron.Expression.text;


/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class SimpleTest extends ATest {
	
	@Test
	public void an() throws Exception {
        
    	new Expression(
			_(THE._, "AA")
		);

    	new Expression(
			_(THE._, "BB", _(HAVE._, "AA", text("a@b")))
		);

    	Expression C = new Expression(
			_(THE._, "CC", _(AN._, "BB"))
		);

        assertAnimo(C, "<the:CC><the:BB><have:AA>a@b</have:AA></the:BB></the:CC>");
	}

	@Test
	public void get() throws Exception {
        
    	new Expression(
			_(THE._, "A")
		);

    	new Expression(
			_(THE._, "B", _(HAVE._, "A", text("a@b")))
		);

    	Expression C = new Expression(
			_(THE._, "C", _(GET._, "A", _(AN._, "B")))
		);

        assertAnimo(C, "<the:C><have:A>a@b</have:A></the:C>");
	}
}