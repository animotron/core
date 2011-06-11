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
package org.animotron.instruction.ml;

import org.animotron.Properties;
import org.animotron.graph.AnimoGraph;
import org.animotron.instruction.AbstractInstruction;
import org.animotron.instruction.InstructionContainer;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;

/**
 * 
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 */
public abstract class ValueInstruction extends AbstractInstruction {
	
	public ValueInstruction(String name, String prefix, String uri) {
		super(name, prefix, uri);
	}
	
	public ValueInstruction(String name, InstructionContainer container) {
		super(name, container);
	}

	@Override
	public Node build(Node parent, String prefix, String ns, String name, Node value, int order) {
		Relationship r = parent.createRelationshipTo(value, relationshipType());
		AnimoGraph.order(r, order);
		return null;
	}
	
	@Override
	public String name(Relationship r){
		return null;
	}
	
	@Override
	public String namespace(Relationship r){
		return null;
	}
	
	@Override
	public String value(Relationship r){
		return Properties.VALUE.get(r.getEndNode());
	}
}