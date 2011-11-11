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
package org.animotron.games;

import org.animotron.ATest;
import org.animotron.expression.JExpression;
import org.animotron.statement.math.MUL;
import org.animotron.statement.math.SUM;
import org.animotron.statement.operator.AN;
import org.animotron.statement.operator.PTRN;
import org.animotron.statement.operator.Q;
import org.animotron.statement.operator.THE;
import org.animotron.statement.query.GET;
import org.animotron.statement.relation.HAVE;

import static org.animotron.expression.JExpression._;
import static org.animotron.expression.JExpression.text;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class Factorial extends ATest {

	//TODO @Test
	public void factorial() throws Exception {
		
    	new JExpression(
			_(THE._, "factorial", 
				_(HAVE._, "number", 
					_(PTRN._, "number", 
						_(Q._, "N0", text("Q:N1")),
						_(Q._, "N0>", 
							_(MUL._,
								_(GET._, "number"),
								_(AN._, "factorial", _(SUM._, _(GET._, "number"), _(Q._, "N-1")))
							)
						) 
					)
				)
			)
		);
	}
}
