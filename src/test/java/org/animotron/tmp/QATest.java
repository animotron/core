/*
 *  Copyright (C) 2012 The Animo Project
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

import javolution.util.FastSet;
import org.junit.Ignore;
import org.junit.Test;

import java.io.IOException;

import static org.junit.Assert.assertEquals;

/**
 * @author Ferenc Kovacs
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class QATest extends ATest {

	@Test
	public void test01() throws IOException {
		
		//properties
		String system = _("system");

		String name = _("name");

		//object 
		System.out.println(
			_("Ann", system, name+" 'Ann'")
		);
		
		sleep(1);
		
		question("what is the name of the system?", "Ann.", "The name of the system is a Ann.");
	}
	
	@Test
	@Ignore
	public void test02() throws IOException {
		
		//properties
		String participant = _("participant");

		String name = _("name");

		String system = _("system");
		String human = _("human");

		//object 
		_("Ann", system, name+" 'Ann'", participant);
		
		_("Ferenc Kovacs", participant, human);
		_("Evgeny Gazdovsky", participant, human);
		_("Dmitriy Shabanov", participant, human);

		_("We");
		
		//relations
		
		//Questions
		_("What");
		_("Who");
		_("How");
		
		//Definition: one for machine, one for user
		
		question("What is the name of the system?", "Ann.", "The name of the system is a Ann.");
		
		
		question("What is the purpose of the system?", "to create a systems description through dialog between the participants.");
		question("What is the form of the system's description?", "dialog", "dialog between the participants");
		question("Who are the participants?", "We", "Ann, Ferenc Kovacs, Evgeny Gazdovsky, Dmitriy Shabanov");
//		question("How does the look like for this passage?", "");
//		question("How do we interpret the code in semantic terms?", "");
	}

	private String question(String message, String ... answers) throws IOException {
		Brain brain = Brain.parse(message);

		FastSet<Brain.MentalObject> state = brain.mentalState;
		assertEquals(1, state.size());
		
		Brain.MentalObject mo = state.valueOf(state.head().getNext());
		assertEquals(2, mo.steps.size());
		
		assertAnimo(mo.steps.get(0), "");

		return "";
	}

}
