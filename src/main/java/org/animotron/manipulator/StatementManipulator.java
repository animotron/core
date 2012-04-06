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
package org.animotron.manipulator;

import org.animotron.statement.Statement;
import org.animotron.statement.Statements;
import org.neo4j.graphdb.Relationship;

/**
 * Manipulations based on statement's behavior.
 * 
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public abstract class StatementManipulator extends Manipulator {

	/**
	 * getThe statement's callback.
	 * 
	 * @param statement
	 * @param op
	 */
	protected abstract OnQuestion onQuestion(Statement statement, Relationship op);

	/**
	 * Should manipulator go this direction?
	 * 
	 * @param statement
	 * @return
	 */
	protected abstract boolean canGo(Statement statement);

	public final OnQuestion onQuestion(final Relationship op) {
		if (op != null) {
			final Statement statement = Statements.relationshipType(op);
			if (canGo(statement))
				return onQuestion(statement, op);
			else 
				return null;
		}
		
		return super.onQuestion(op);
	}

}