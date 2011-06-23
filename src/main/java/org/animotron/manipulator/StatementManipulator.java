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
package org.animotron.manipulator;

import org.animotron.Statement;
import org.animotron.Statements;
import org.jetlang.channels.Subscribable;
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
	 * get statement's callback.
	 * 
	 * @param statement
	 * @param op
	 */
	protected abstract Subscribable<PFlow> onQuestion(Statement statement, Relationship op);

	/**
	 * Should manipulator go this direction?
	 * 
	 * @param statement
	 * @return
	 */
	protected abstract boolean canGo(Statement statement);

	protected final Subscribable<PFlow> onQuestion(final Relationship op) {
		final Statement statement = Statements.relationshipType(op.getType());
		if (canGo(statement))
			return onQuestion(statement, op);
		
		return super.onQuestion(op);
	}

}