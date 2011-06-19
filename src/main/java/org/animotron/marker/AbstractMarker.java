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
package org.animotron.marker;

import static org.animotron.graph.AnimoGraph.getOrCreateNode;
import static org.animotron.graph.AnimoGraph.getROOT;

import org.animotron.graph.AnimoGraph;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.RelationshipType;
import org.neo4j.graphdb.Transaction;

/**
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public abstract class AbstractMarker implements Marker {

	private Node root;
	private Relationship state;
	private RelationshipType type;
	
	public AbstractMarker(final RelationshipType type) {
		this.type = type;
		Transaction tx = AnimoGraph.beginTx();
		try {
			root = getOrCreateNode(getROOT(), type);
		} finally {
			AnimoGraph.finishTx(tx);
		}
		
	}

	@Override
	public Node root() {
		return root;
	}
	
	@Override
	public RelationshipType type() {
		return type;
	}
	
	@Override
	public void mark(Node node) {
		state = root.createRelationshipTo(node, type());
	}

	@Override
	public void drop() {
		state.delete();
	}

}
