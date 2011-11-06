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
import org.animotron.statement.relation.HAVE;
import org.animotron.statement.relation.HAVE_NOT;
import org.animotron.statement.relation.IS;
import org.animotron.statement.relation.IS_NOT;
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
				_(IS._, "person"),
				_(IS._, "female")
		);

		//Man ≡ Person ⊓ ¬Woman
		new JExpression(
			_(THE._, "man"),
				_(IS._, "person"),
				_(IS_NOT._, "woman")
		);

		//Mother ≡ Woman ⊓ ∃hasChild.Person
		new JExpression(
			_(THE._, "mother"),
				_(IS._, "woman"),
				_(HAVE._, "child", _(IS._, "person"))
		);
		
		//Father ≡ Man ⊓ ∃hasChild.Person
		new JExpression(
			_(THE._, "father"),
				_(IS._, "man"),
				_(HAVE._, "child", _(IS._, "person"))
		);

		//TODO: find a way to describe ⊔ ....
		//the:parents` is:mother` is:father` is:parent?
		
 		//Parent ≡ Mother ⊔ Father.
		new JExpression(
			_(THE._, "parent"),
				_(IS._, "mother"),
				_(IS._, "father")
		);
		
		//Grandmother ≡ Mother ⊓ ∃hasChild.Parent
		new JExpression(
			_(THE._, "grandmother"),
				_(IS._, "mother"),
				_(HAVE._, "child", _(IS._, "parent"))
		);

		//MotherWith3Children  ≡ Mother ⊓ >= 3 hasChild
		new JExpression(
			_(THE._, "motherWith3Children"),
				_(ANY._, "mother", _(GE._, _(COUNT._, "child"), _(Q._, "N3")))
		);

		//MotherWithoutDaughter ≡ Mother ⊓ ∀hasChild.¬Woman
		new JExpression(
			_(THE._, "motherWithoutDaughter"),
				_(ANY._, "mother", _(HAVE_NOT._, "child", _(IS._, "woman")))
		);

		//Wife  ≡ Woman ⊓ ∃hasHusband.Man
		new JExpression(
			_(THE._, "wife"),
				_(IS._, "woman", 
				_(HAVE._, "husband", _(IS._, "man")))
		);
		

		new JExpression(
			_(THE._, "personA"),
				_(IS._, "man")
		);
		
		new JExpression(
			_(THE._, "personB"),
				_(IS._, "woman"),
				_(HAVE._, "child", _(AN._, "personA"))
		);

		//TODO: Is personA mother? (personA is mother => is:mother an:personA)
		JExpression a = new JExpression(
			_(THE._, "a"),
				_(AN._, "Question", _(IS._, "mother", _(AN._, "personB")))
				//is:personB an:mother?
				//eq an:personB; an:mother?
		);
		
		assertAnimoResult(a, "the a the yes.");
		
	}
}
