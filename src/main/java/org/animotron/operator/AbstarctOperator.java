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

import org.animotron.Properties;
import org.animotron.graph.AnimoGraph;
import org.animotron.graph.AnimoRelationshipType;
import org.animotron.io.PipedOutputObjectStream;
import org.neo4j.graphdb.Direction;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.RelationshipType;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public abstract class AbstarctOperator implements Operator {
	
	private static final RelationshipType REF = AnimoRelationshipType.get("REF"); 
	
	private String prefix;
	private String uri;
	private RelationshipType relationshipType;
	private RelationshipType resultRelationshipType = null;
	
	public AbstarctOperator(String prefix, String uri) {
		this(prefix, uri, prefix.toUpperCase());
	}
	
	public AbstarctOperator(String prefix, String uri, String relationshipType) {
		this.prefix = prefix;
		this.uri = uri;
		this.relationshipType = AnimoRelationshipType.get(relationshipType);
	}

	public AbstarctOperator(String prefix, String uri, String relationshipType, String resultRelationshipType) {
		this.prefix = prefix;
		this.uri = uri;
		this.relationshipType = AnimoRelationshipType.get(relationshipType);
		this.resultRelationshipType = AnimoRelationshipType.get(resultRelationshipType);
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
	
	@Override
	public RelationshipType resultRelationshipType() {
		return resultRelationshipType;
	}

	@Override
	public Node build(Node parent, String prefix, String ns, String name, Node value, int order) {
		Node child = AnimoGraph.createNode();
		Relationship r = parent.createRelationshipTo(child, relationshipType);
		AnimoGraph.order(r, order);
		child.createRelationshipTo(THE._.getOrCreate(name), REF);
		return child;
	}

	
	public void eval(Relationship op, PipedOutputObjectStream ot, boolean isLast) throws IOException {
		System.out.println("empty eval @"+this.getClass());
		ot.write(op);
		ot.close();
	}
	
	@Override
	public String name(Relationship r) {
		Node node = r.getEndNode().getSingleRelationship(REF, Direction.OUTGOING).getEndNode(); 
		return Properties.NAME.get(node);
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
		return prefix;
	}
	
	@Override
	public String qname(Relationship r) {
		return prefix(r) + ":" + name(r);
	}

	
}
