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
package org.animotron.statement.string;

import org.animotron.ATest;
import org.animotron.Expression;
import org.animotron.statement.operator.AN;
import org.animotron.statement.operator.THE;
import org.animotron.statement.query.GET;
import org.animotron.statement.relation.HAVE;
import org.junit.Test;

import static org.animotron.Expression._;
import static org.animotron.Expression.text;

/**
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class AfterLastTest extends ATest {
	
	@Test
	public void testAfterLast() throws Exception {
        System.out.println("Test 'AfterLast' ...");
        
    	new Expression(
			Expression._(THE._, "A", _(HAVE._, "B", text("x.y.z")))
		);
    	
    	Expression C = new Expression(
			_(THE._, "C", _(AfterLast._, text("."), _(GET._, "B", Expression._(AN._, "A"))))
		);
    	
        assertAnimoResult(C, "the C \"z\"");

        //System.out.println("done.");
	}
	
}
