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

import static org.animotron.Properties.NAME;
import static org.animotron.graph.AnimoGraph.order;

import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;


/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public abstract class Relation extends AbstarctOperator {
	
	public Relation(String prefix, String uri) {
		super(prefix, uri);
	}

	@Override
	public Node build(Node parent, String prefix, String ns, String name, Node value, int order) {
		Node target = THE._.getOrCreate(name);
		if (!parent.equals(target)) {
			Relationship r = parent.createRelationshipTo(target, relationshipType());
			order(r, order);

		}
		return null;
	}
	
	@Override
	public String name(Relationship r){
		return NAME.get(r.getEndNode());
	}
	
}
