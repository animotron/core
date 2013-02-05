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
package org.animotron.tmp;

import org.junit.Ignore;
import org.junit.Test;

/**
 * @author Ferenc Kovacs
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class NounPlusNounTest extends ATest {

	/*
	 * 1. Problems: noun plus noun may refer to a single object e.g. and be in a productive relation (not to be separated) 
	 * or in an additive relation where noun1 is an object and noun2 is a property that object has, but not a quality of that object, 
	 * so they are separable example?
	 * 
	 *  driver seat / a specific seat on the front left hand side inside a car on the continent - productive relation , a term
	 *  back seat / a generic seat anywhere this spatial/orientational relation exists between a seat and its relative location
	 *  
	 *  2. yes is the answer 
	 * 
	 */
	@Test
	@Ignore
	public void test_01() throws Throwable {

		testAnimi("driver\n", "driver");
		testAnimi("back\n", "back");
		testAnimi("seat\n", "seat");
		
		testAnimi("driver seat\n", "driver seat");
		testAnimi("back seat\n", "back seat");
		
		//XXX: to be continue
	}
	
}
