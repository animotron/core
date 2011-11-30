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
package org.animotron.games;

import org.animotron.ATest;
import org.animotron.expression.JExpression;
import org.animotron.statement.instruction.COUNT;
import org.animotron.statement.instruction.compare.GE;
import org.animotron.statement.operator.AN;
import org.animotron.statement.operator.Q;
import org.animotron.statement.operator.THE;
import org.animotron.statement.query.ANY;
import org.junit.Ignore;
import org.junit.Test;

import static org.animotron.expression.JExpression._;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class DescriptionLogicTest extends ATest {

	@Test
    @Ignore
	public void famaly() throws Exception {
		//TODO: Person ≡ Female ⊔ Male?
		
		//Woman ≡ Person ⊓ Female
		new JExpression(
			_(THE._, "woman"),
				_(AN._, "person"),
				_(AN._, "female")
		);

		//Man ≡ Person ⊓ ¬Woman
		new JExpression(
			_(THE._, "man"),
				_(AN._, "person")/*,
				_(IS_NOT._, "woman")*/
		);

		//Mother ≡ Woman ⊓ ∃hasChild.Person
		new JExpression(
			_(THE._, "mother"),
				_(AN._, "woman"),
				_(AN._, "child", _(AN._, "person"))
		);
		
		//Father ≡ Man ⊓ ∃hasChild.Person
		new JExpression(
			_(THE._, "father"),
				_(AN._, "man"),
				_(AN._, "child", _(AN._, "person"))
		);

		//TODO: find a way to describe ⊔ ....
		//the:parents` is:mother` is:father` is:parent?
		
 		//Parent ≡ Mother ⊔ Father.
		new JExpression(
			_(THE._, "parent"),
				_(AN._, "mother"),
				_(AN._, "father")
		);
		
		//Grandmother ≡ Mother ⊓ ∃hasChild.Parent
		new JExpression(
			_(THE._, "grandmother"),
				_(AN._, "mother"),
				_(AN._, "child", _(AN._, "parent"))
		);

		//MotherWith3Children  ≡ Mother ⊓ >= 3 hasChild
		new JExpression(
			_(THE._, "motherWith3Children"),
				_(ANY._, "mother", _(GE._, _(COUNT._, "child"), _(Q._, "N3")))
		);

		//MotherWithoutDaughter ≡ Mother ⊓ ∀hasChild.¬Woman
		new JExpression(
			_(THE._, "motherWithoutDaughter"),
				_(ANY._, "mother"/*, _(HAVE_NOT._, "child", _(AN._, "woman"))*/)
		);

		//Wife  ≡ Woman ⊓ ∃hasHusband.Man
		new JExpression(
			_(THE._, "wife"),
				_(AN._, "woman",
				_(AN._, "husband", _(AN._, "man")))
		);
		

		new JExpression(
			_(THE._, "personA"),
				_(AN._, "man")
		);
		
		new JExpression(
			_(THE._, "personB"),
				_(AN._, "woman"),
				_(AN._, "child", _(AN._, "personA"))
		);

		//TODO: Is personA mother? (personA is mother => is:mother an:personA)
		JExpression a = new JExpression(
			_(THE._, "a"),
				_(AN._, "Question", _(AN._, "mother", _(AN._, "personB")))
				//is:personB an:mother?
				//eq an:personB; an:mother?
		);
		
		assertAnimoResult(a, "the a the yes.");
		
	}

	public void famaly_02() throws Exception {
		testAnimo("the joe (father john) (child john) (son john) (parent john).");
		testAnimo("the john (father joe) (child joe) (son joe) (parent joe).");
	}

	public void famaly_03() throws Exception {
		testAnimo("the parent child.");
		testAnimo("the child parent.");

		testAnimo("the john (child joe).");

		assertAnimoResult("john", "the joe parent john.");
	}
}
