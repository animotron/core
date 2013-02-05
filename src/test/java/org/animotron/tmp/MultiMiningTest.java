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

import org.junit.Test;

import static org.animotron.expression.AnimoExpression.__;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class MultiMiningTest extends ATest {

	@Test
	public void test_01() throws Throwable {
		__(
			"def chf (label (ru \"шеф\") (en \"chief\")).",
			
			//шеф == патрон
			"def chf-pat (label (ru \"патрон\") (en \"patron\")) (chf).",
			"def car (label (ru \"патрон\") (en \"cartridge\")).",
				
			// магазин == магазин
			"def mag (label (ru \"магазин\") (en \"magazine\")).",
			"def sho (label (ru \"магазин\") (en \"shop\")).",
			
			"def ent (label (ru \"вошёл\") (en \"enter\")).",
			
			"def experience-1 (car ent mag).", //патрон вошёл-в магазин
			"def experience-2 (chf ent sho)." //chief enter shop
		);
	}
}