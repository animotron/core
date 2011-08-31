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

import org.animotron.statement.instruction.Instruction;
import org.animotron.statement.operator.Cachable;
import org.animotron.statement.operator.Result;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;

import static org.animotron.Properties.NAME;
import static org.animotron.graph.AnimoGraph.createNode;
import static org.animotron.graph.AnimoGraph.order;

/**
 * Instruction 'ml:element'.
 * 
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 */
public class ELEMENT extends Instruction implements Cachable, Result {
	
	public static final ELEMENT _ = new ELEMENT();
	
	private ELEMENT() { super("element"); }
	
	@Override
	public Relationship build(Node parent, String name, Node value, int order, boolean ignoreNotFound) {
		Node child = createNode();
		Relationship r = parent.createRelationshipTo(child, relationshipType());
		order(r, order);
		NAME.set(child, name);
		return r;
	}
	
	@Override
	public String name(Relationship r){
		return NAME.get(r.getEndNode());
	}
	
}