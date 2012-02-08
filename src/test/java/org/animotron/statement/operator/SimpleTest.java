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
package org.animotron.statement.operator;

import org.animotron.ATest;
import org.animotron.expression.JExpression;
import org.animotron.statement.query.GET;
import org.junit.Test;

import static org.animotron.expression.JExpression._;
import static org.animotron.expression.JExpression.__;
import static org.animotron.expression.JExpression.value;


/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class SimpleTest extends ATest {
	
	@Test
	public void an() throws Exception {
        
    	__(
            new JExpression(
                    _(THE._, "AA")
            ),
            new JExpression(
                    _(THE._, "BB", _(AN._, "AA", value("a@b")))
            )
        );

    	JExpression C = new JExpression(
			_(THE._, "CC", _(AN._, "BB"))
		);
        assertAnimoResultOneStep(C, "the CC BB AA \"a@b\".");
	}

	@Test
	public void get() throws Exception {
        
    	__(
            new JExpression(
                    _(THE._, "A")
            ),
            new JExpression(
                    _(THE._, "B", _(AN._, "A", value("a@b")))
            )
        );

    	JExpression C = new JExpression(
			_(THE._, "C", _(GET._, "A", _(AN._, "B")))
		);
        assertAnimoResult(C, "the C A \"a@b\".");
	}
}