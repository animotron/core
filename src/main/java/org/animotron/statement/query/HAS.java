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
package org.animotron.statement.query;

import org.animotron.manipulator.PFlow;
import org.animotron.statement.operator.Operator;
import org.animotron.statement.operator.Predicate;
import org.animotron.statement.operator.Query;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;

/**
 * Query operator 'HAS'.
 * 
 * Has HAVE relation?
 * 
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 */
public class HAS extends Operator implements Query, Predicate {
	
	public static final HAS _ = new HAS();
	
	private HAS() { super("has"); }

	@Override
	public boolean filter(PFlow pf, Relationship op, Node ref) {
		// TODO Auto-generated method stub
		return false;
	}
	
}
