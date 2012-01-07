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
import org.junit.Ignore;
import org.junit.Test;

import static org.animotron.expression.JExpression._;
import static org.animotron.expression.JExpression.value;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class TheTest extends ATest {
	
    @Test
    public void testTHE() throws Exception {

        JExpression A = new JExpression(
            _(THE._, "A", _(THE._, "B", _(THE._, "C")))
        );
        assertAnimoResult(A, "the A the B the C.");
        assertAnimo(A, "the A the B the C.");
    }

	@Test
    @Ignore
	public void getFromPFlow() throws Exception {
        System.out.println("Test the 'THE' ...");
        
    	JExpression A = new JExpression(
			_(THE._, "A", _(AN._, "B", _(THE._, "C", _(AN._, "D", value(".")))))
		);
    	
    	new JExpression(
			_(THE._, "C", _(AN._, "D", value(".")))
		);
        
    	JExpression E = new JExpression(
			_(THE._, "E", _(AN._, "C"))
		);
        	
        assertAnimoResult(A, "the A B the C D \".\".");
        assertAnimoResult(E, "the E the C D \".\".");
	}
}