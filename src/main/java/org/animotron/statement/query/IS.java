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
package org.animotron.statement.query;

import java.io.IOException;
import java.util.Set;

import javolution.util.FastSet;

import org.animotron.manipulator.PFlow;
import org.animotron.statement.operator.Operator;
import org.animotron.statement.operator.Predicate;
import org.animotron.statement.operator.Utils;
import org.neo4j.graphdb.Relationship;

/**
 * Predicate '?IS'.
 * 
 * Return "true" if object is of given type/class
 * 
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 */
public class IS extends Operator implements Predicate {

	public static final IS _ = new IS();

	private IS() { super("?is"); }

	@Override
	public boolean filter(PFlow pf, Relationship op, Relationship ref) throws InterruptedException, IOException {

		System.out.println("?is");
		System.out.println(ref);

		FastSet<Relationship> thes = FastSet.newInstance();
		try {
			Utils.getTHELikeBag(pf, pf.getVector().question(op), thes);
			
			for (FastSet.Record rc = thes.head(), end = thes.tail(); (rc = rc.getNext()) != end;) {
				Relationship the = thes.valueOf(rc);
				
				System.out.println(the);
				
				if (the.getEndNode().equals(ref.getEndNode()))
					return true;
			}
		} finally {
			FastSet.recycle(thes);
		}
		return false;
	}

	@Override
	public Set<Relationship> getExpected(PFlow pf, Relationship op) throws InterruptedException, IOException {
		return null;
	}
}