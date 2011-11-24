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
		testAnimo("the user1 (is user) (have name \"user1\").");

		testAnimo("the item1 (is item) (have name \"item1\").");
		
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
		testAnimo("the user1 (is user) (have name \"user1\").");

		testAnimo("the item1 (is item) (have name \"item1\").");
		
		testAnimo("the f_con user1,item1.");
		testAnimo("the f_get get user's name.");
		
		assertAnimoResult(
            new AnimoExpression("f_get, f_con."),
            "have name \"user1\""
        );
	}
}
