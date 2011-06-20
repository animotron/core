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

import static org.animotron.graph.AnimoGraph.createNode;
import static org.animotron.graph.AnimoGraph.order;

import java.io.IOException;

import org.animotron.graph.AnimoRelationshipType;
import org.animotron.io.PipedOutput;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;
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
	//private InstructionContainer container;
	
	private RelationshipType relationshipType;
	
	public AbstractInstruction(final String name, final String prefix, final String uri) {
		this.name = name;
		this.prefix = prefix;
		this.uri = uri;

		this.relationshipType = 
			AnimoRelationshipType.get(prefix.toUpperCase(), name.toUpperCase());
	}
	
	public AbstractInstruction(final String name, final InstructionContainer container) {
		this(name, container.name(), container.namespace());
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
	public RelationshipType relationshipType() {
		return relationshipType;
	}
	
	@Override
	public RelationshipType resultRelationshipType() {
		return null;
	}

	@Override
	public Relationship build(Node parent, String prefix, String ns, String name, Node value, int order) {
		Node child = createNode();
		Relationship r = parent.createRelationshipTo(child, relationshipType);
		order(r, order);
		return r;
	}
	
	@Override
	public String name(Relationship r) {
		return name();
	}
	
	@Override
	public String namespace(Relationship r) {
		return namespace();
	}
	
	@Override
	public String value(Relationship r) {
		return null;
	}
	
	@Override
	public String prefix(Relationship r) {
		return prefix();
	}

	@Override
	public String qname(Relationship r) {
		return prefix(r) + ":" + name(r);
	}

	public void eval(Relationship op, PipedOutput ot, boolean isLast) throws IOException {
		System.out.println("empty eval @"+this.getClass());
		ot.write(op);
	}
}
