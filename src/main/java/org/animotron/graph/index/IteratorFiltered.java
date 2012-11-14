/*
 *  Copyright (C) 2012 The Animo Project
 *  http://animotron.org
 *
 *  This file is part of Animi.
 *
 *  Animotron is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU Affero General Public License as
 *  published by the Free Software Foundation, either version 3 of
 *  the License, or (at your option) any later version.
 *
 *  Animotron is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Affero General Public License for more details.
 *
 *  You should have received a copy of
 *  the GNU Affero General Public License along with Animotron.
 *  If not, see <http://www.gnu.org/licenses/>.
 */
package org.animotron.graph.index;

import java.util.Iterator;
import java.util.Set;

import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.RelationshipType;
import org.neo4j.graphdb.index.IndexHits;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class IteratorFiltered implements Iterator<Relationship> {

	IndexHits<Relationship> hits;
	Set<RelationshipType> allowed;
	
	Relationship next = null;
	
	public IteratorFiltered(Node node, Set<RelationshipType> allowed) {
		hits = Order._.queryDown(node);
		this.allowed = allowed;
		
		getNext();
	}
	
	@Override
	public boolean hasNext() {
		return next != null;
	}

	@Override
	public Relationship next() {
		final Relationship current = next;
		
		getNext();
		
		return current;
	}
	
	private void getNext() {
		
		while (hits.hasNext()) {
			next = hits.next();
			for (RelationshipType type : allowed)
				if (next.isType(type))
					return;
		}
		hits.close();
		next = null;
	}

	@Override
	public void remove() {
	}
}
