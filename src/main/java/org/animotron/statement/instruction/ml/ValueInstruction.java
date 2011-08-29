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
package org.animotron.statement.instruction.ml;

import org.animotron.statement.AbstractStatement;
import org.animotron.statement.operator.Result;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;

import static org.animotron.graph.AnimoGraph.order;

/**
 * 
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 */
public abstract class ValueInstruction extends AbstractStatement implements Result {
	
	public ValueInstruction(String name) {
		super(name);
	}

	@Override
	public Relationship build(Node parent, String name, Node value, int order, boolean ignoreNotFound) {
		Relationship r = parent.createRelationshipTo(value, relationshipType());
		order(r, order);
		return r;
	}
	
	@Override
	public String name(Relationship r){
		return null;
	}
	
}