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
import org.animotron.expression.AnimoExpression;
import org.junit.Ignore;
import org.junit.Test;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class PossessiveCaseTest extends ATest {

	@Test
	@Ignore
	public void inContext() throws Exception {
		testAnimo("the user1 (user) (name \"user1\").");

		testAnimo("the item1 (item) (name \"item1\").");
		
		//XXX: fix!
		//testAnimo("get (user's name) (user1, item1).", "get( user's name) (user1, item1).");
		testAnimo("user's get name user1, item1.", " user's get name user1, item1.");

		assertAnimoResult(
            new AnimoExpression("user's get name user1, item1."),
            "have name \"user1\"."
        );
	}

	@Test
	@Ignore
	public void inPFlow() throws Exception {
		testAnimo("the user1 (user) (name \"user1\").");

		testAnimo("the item1 (item) (name \"item1\").");
		
		testAnimo("the f_con user1,item1.");
		testAnimo("the f_get get user's name.");
		
		assertAnimoResult(
            new AnimoExpression("f_get, f_con."),
            "have name \"user1\""
        );
	}
}
