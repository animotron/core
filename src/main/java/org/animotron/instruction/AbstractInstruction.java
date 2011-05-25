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
package org.animotron.instruction;

import org.animotron.Container;
import org.animotron.graph.AnimoGraph;
import org.animotron.graph.AnimoRelationshipType;
import org.animotron.operator.Operator;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.RelationshipType;

/**
 * Abstract instruction.
 * 
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 */
public abstract class AbstractInstruction implements Instruction {
	
	private final String name;
	private final String prefix;
	private final String uri;
	private final Container container;
	
	private RelationshipType relationshipType;
	
	public AbstractInstruction(final Container container, final String name) {
		this.name = name;
		this.prefix = container.name();
		this.uri = container.namespace();
		this.container = container;
		this.relationshipType = 
			AnimoRelationshipType.get(prefix.toUpperCase(), name.toUpperCase());
	}
	
	public AbstractInstruction(Operator container, String name) {
		this.name = name;
		this.prefix = container.name();
		this.uri = container.namespace();
		this.container = container;
		this.relationshipType = container.relationshipType(); 
	}
	
	@Override
	public String name() {
		return name;
	}

	@Override
	public String prefix() {
		return prefix;
	}

	@Override
	public String namespace() {
		return uri;
	}

	@Override
	public Container container() {
		return container;
	}

	@Override
	public RelationshipType relationshipType() {
		return relationshipType;
	}
	
	@Override
	public Node build(Node parent){
		return AnimoGraph.createNode(parent, relationshipType);
	}
	
}
