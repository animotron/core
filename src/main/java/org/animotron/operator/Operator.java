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
package org.animotron.operator;

import java.io.IOException;

import org.animotron.Statement;
import org.animotron.graph.AnimoGraph;
import org.animotron.graph.AnimoRelationshipType;
import org.animotron.io.PipedOutputObjectStream;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.RelationshipType;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public abstract class Operator implements Statement {
	
	private String prefix;
	private String uri;
	private RelationshipType relationshipType;
	
	public Operator(String prefix, String uri) {
		this.prefix = prefix;
		this.uri = uri;
		this.relationshipType = AnimoRelationshipType.get(prefix.toUpperCase());
	}
	
	@Override
	public String name() {
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
	
	public Node build(Node parent, String name){
		Node node = AnimoGraph.createNode(parent, relationshipType);
		node.createRelationshipTo(THE.getInstance().getOrCreate(name), AnimoRelationshipType.get("REF"));
		return node;
	}

	
	public void eval(Relationship op, PipedOutputObjectStream ot, boolean isLast) throws IOException {
		System.out.println("empty eval @"+this.getClass());
	}
	
}
