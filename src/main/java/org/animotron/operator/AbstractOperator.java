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

import org.animotron.exception.ENotFound;
import org.animotron.graph.AnimoGraph;
import org.animotron.graph.AnimoRelationshipType;
import org.animotron.graph.GraphOperation;
import org.animotron.inmemory.InMemoryRelationship;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.RelationshipType;

import static org.animotron.Properties.NAME;
import static org.animotron.Properties.RID;
import static org.animotron.graph.AnimoGraph.createNode;
import static org.animotron.graph.AnimoGraph.order;
import static org.animotron.graph.RelationshipTypes.RESULT;
import static org.neo4j.graphdb.Direction.OUTGOING;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public abstract class AbstractOperator implements Operator {
	
	private static final RelationshipType REF = AnimoRelationshipType.get("REF"); 
	
	private String prefix;
	private String uri;
	public final RelationshipType relationshipType;
	public final String rType;
	
	public AbstractOperator(String prefix, String uri) {
		this(prefix, uri, prefix.toUpperCase());
	}
	
	public AbstractOperator(String prefix, String uri, String relationshipType) {
		this.prefix = prefix;
		this.uri = uri;
		this.relationshipType = AnimoRelationshipType.get(relationshipType);
		rType = this.relationshipType.name(); 
	}

	public AbstractOperator(String prefix, String uri, String relationshipType, String resultRelationshipType) {
		this.prefix = prefix;
		this.uri = uri;
		this.relationshipType = AnimoRelationshipType.get(relationshipType);
		rType = this.relationshipType.name(); 
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
	public Relationship build(Node parent, String prefix, String ns, String name, Node value, int order, boolean ignoreNotFound) throws ENotFound {
		Node child = createNode();
		Relationship r = parent.createRelationshipTo(child, relationshipType);
		order(r, order);
		child.createRelationshipTo(THE._.getOrCreate(name, ignoreNotFound).getEndNode(), REF);
		return r;
	}

	
	protected Relationship createResult(final Node node, final Relationship r) {
		return AnimoGraph.execute(new GraphOperation<Relationship>() {
			@Override
			public Relationship execute() {
				Relationship res = node.createRelationshipTo(r.getEndNode(), RESULT);
				//store to relationship arrow 
				RID.set(res, r.getId());
				
				return res;
			}
		});
	}
	
	protected Relationship createResultInMemory(Node node, Relationship r) {
		Relationship res = new InMemoryRelationship(node, r.getEndNode(), RESULT);
		//store to relationship arrow 
		RID.set(res, r.getId());
		
		return res;
	}

	@Override
	public String name(Relationship r) {
		Node node = r.getEndNode().getSingleRelationship(REF, OUTGOING).getEndNode(); 
		return NAME.get(node);
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