/*
 *  Copyright (C) 2011-2013 The Animo Project
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
import org.animotron.expression.Expression;
import org.junit.Ignore;
import org.junit.Test;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class DescriptionLogicTest extends ATest {

	@Test
    @Ignore
	public void famaly() throws Throwable {
		//TODO: Person ≡ Female ⊔ Male?
		
		//Woman ≡ Person ⊓ Female
        tAnimo("def woman (person) (female).");

		//Man ≡ Person ⊓ ¬Woman
        tAnimo("def man person.");
//        tAnimo("def man (person) (is_not woman).");

		//Mother ≡ Woman ⊓ ∃hasChild.Person
        tAnimo("def mother (woman) (child person).");

		//Father ≡ Man ⊓ ∃hasChild.Person
        tAnimo("def father (man) (child person).");

		//TODO: find a way to describe ⊔ ....
		//the:parents` is:mother` is:father` is:parent?
		
 		//Parent ≡ Mother ⊔ Father.
        tAnimo("def parent (mother) (father).");

		//Grandmother ≡ Mother ⊓ ∃hasChild.Parent
        tAnimo("def grandmother (mother) (child person).");

		//MotherWith3Children  ≡ Mother ⊓ >= 3 hasChild
        tAnimo("def motherWith3Children any mother ge (count child) 3.");

		//MotherWithoutDaughter ≡ Mother ⊓ ∀hasChild.¬Woman
        tAnimo("def motherWithoutDaughter any mother have_not child woman.");

		//Wife  ≡ Woman ⊓ ∃hasHusband.Man
        tAnimo("def wife (woman) (husband man).");


        tAnimo("def personA man.");

        tAnimo("def ppersonB (woman) (child personA).");

		//TODO: Is personA mother? (personA is mother => is:mother an:personA)
        Expression a = tAnimo("def a Question mother personB.");

		assertAnimoResult(a, "a yes.");
		
	}

	public void famaly_02() throws Throwable {
		testAnimo("def joe (father john) (child john) (son john) (parent john).");
		testAnimo("def john (father joe) (child joe) (son joe) (parent joe).");
	}

	public void famaly_03() throws Throwable {
		testAnimo("def parent child.");
		testAnimo("def child parent.");

		testAnimo("def john (child joe).");

		assertAnimoResult("john", "joe parent john.");
	}
}
