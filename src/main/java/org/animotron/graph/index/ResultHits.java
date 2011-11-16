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
package org.animotron.graph.index;

import static org.animotron.Properties.CID;

import org.animotron.graph.AnimoGraph;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.index.IndexHits;
import org.neo4j.index.impl.lucene.AbstractIndexHits;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class ResultHits extends AbstractIndexHits<Relationship[]> implements IndexHits<Relationship[]> {
	
	IndexHits<Relationship> it;
	
	ResultHits(IndexHits<Relationship> hits) {
		it = hits;
	}

	@Override
	public void close() {
		it.close();
	}

	@Override
	public int size() {
		return it.size();
	}

	@Override
	public float currentScore() {
		return it.currentScore();
	}

	@Override
	protected Relationship[] fetchNextOrNull() {
		if (!it.hasNext()) return null;
		
		Relationship r = it.next();
		Relationship c = null;
		try {
			long id = (Long)r.getProperty(CID.name());
			
			c = AnimoGraph.getDb().getRelationshipById(id);
			
		} catch (Exception e) {
		}
		if (c == null)
			return new Relationship[] {r};
		else
			return new Relationship[] {r, c};
	}
}