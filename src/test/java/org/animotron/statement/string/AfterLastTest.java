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
package org.animotron.statement.string;

import org.animotron.ATest;
import org.animotron.expression.JExpression;
import org.animotron.statement.operator.AN;
import org.animotron.statement.operator.THE;
import org.animotron.statement.query.GET;
import org.junit.Test;

import static org.animotron.expression.JExpression._;
import static org.animotron.expression.JExpression.value;

/**
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class AfterLastTest extends ATest {
	
	@Test
	public void testAfterLast() throws Throwable {
        System.out.println("Test 'AfterLast' ...");
        
    	JExpression.__(new JExpression(
                JExpression._(THE._, "A", _(AN._, "B", value("x.y.z")))
        ));
    	
    	JExpression C = new JExpression(
			_(THE._, "C", _(AFTER_LAST._, value("."), _(GET._, "B", JExpression._(AN._, "A"))))
		);
    	
        assertAnimoResult(C, "the C \"z\".");

        //System.out.println("done.");
	}
	
}
