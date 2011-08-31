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
package org.animotron.statement.operator;

import org.animotron.exception.ENotFound;
import org.animotron.graph.AnimoRelationshipType;
import org.animotron.statement.AbstractStatement;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.RelationshipType;

import static org.animotron.Properties.NAME;
import static org.animotron.graph.AnimoGraph.createNode;
import static org.animotron.graph.AnimoGraph.order;
import static org.neo4j.graphdb.Direction.OUTGOING;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public abstract class Operator extends AbstractStatement {

	private static final RelationshipType REF = AnimoRelationshipType.get("REF");

    public Operator(String name) {
        super(name);
    }

    @Override
	public Relationship build(Node parent, String name, Node value, int order, boolean ignoreNotFound) throws ENotFound {
		Node child = createNode();
		Relationship r = parent.createRelationshipTo(child, relationshipType());
		order(r, order);
		child.createRelationshipTo(THE._.getOrCreate(name, ignoreNotFound).getEndNode(), REF);
		return r;
	}

	@Override
	public String name(Relationship r) {
		Node node = r.getEndNode().getSingleRelationship(REF, OUTGOING).getEndNode(); 
		return NAME.get(node);
	}
	
}