/*
 *  Copyright (C) 2011-2012 The Animo Project
 *  http://animotron.org
 *  	
 *  This file is part of Animotron.
 *  
 *  Animotron is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU Affero General Public License as 
 *  published by the Free Software Foundation, either version 3 of 
 *  the License, or (at your option) any later version.
 *  
 *  Animotron is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Affero General Public License for more details.
 *  
 *  You should have received a copy of 
 *  the GNU Affero General Public License along with Animotron.  
 *  If not, see <http://www.gnu.org/licenses/>.
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

import static org.animotron.expression.JExpression._;
import static org.animotron.expression.JExpression.value;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class Factorial extends ATest {

	//TODO @Test
	public void factorial() throws Exception {
		
    	new JExpression(
			_(THE._, "factorial", 
				_(AN._, "number",
					_(PTRN._, "number", 
						_(Q._, "N0", value("Q:N1")),
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
