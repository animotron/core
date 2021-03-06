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
package org.animotron.statement.compare;

import org.animotron.manipulator.PFlow;
import org.animotron.statement.operator.Operator;
import org.animotron.statement.operator.Predicate;
import org.neo4j.graphdb.Relationship;

import java.io.IOException;
import java.util.Set;

/**
 * Compare operator 'LE'.
 * 
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 */
public class LE extends Operator implements Predicate {
	
	public static final LE _ = new LE();
	
	private LE() { super("le", "<="); }

	@Override
	public boolean filter(PFlow pf, Relationship op, Relationship ref) {
		// TODO Auto-generated method stub
		return false;
	}
	
	@Override
	public Set<Relationship> getExpected(PFlow pf, Relationship op) throws InterruptedException, IOException {
		//XXX: code
		return null;
	}
}