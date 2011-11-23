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
import org.animotron.statement.relation.IC;
import org.animotron.statement.relation.IS;
import org.junit.Test;

import static org.animotron.expression.JExpression._;
import static org.animotron.expression.JExpression.text;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class ICTest extends ATest {
	
	@Test
	public void testIC() throws Exception {
        
    	JExpression.__(
                new JExpression(
                        _(THE._, "A")
                ),
                new JExpression(
                        _(THE._, "B", _(IC._, "A", text(".")))
                ),
                new JExpression(
                        _(THE._, "C", _(IS._, "B"))
                )
        );

    	JExpression D = new JExpression(
			_(THE._, "D", _(GET._, "A", _(AN._, "C")))
		);

        assertStringResult(D, ".");
        //XXX: assertAnimoResult(D, "the D have A \".\".");
        assertAnimoResult(D, "the D ic A \".\".");
	}
}