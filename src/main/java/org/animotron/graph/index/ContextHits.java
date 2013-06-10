/*
 *  Copyright (C) 2011-2013 The Animo Project
 *  http://animotron.org
 *  	
 *  This file is part of Animotron.
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

import org.animotron.statement.operator.REF;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.index.IndexHits;
import org.neo4j.index.impl.lucene.AbstractIndexHits;

import static org.animotron.graph.Properties.NAME;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class ContextHits extends AbstractIndexHits<Relationship> implements IndexHits<Relationship> {
	
	IndexHits<Relationship> it;
	boolean first = true;
	
	ContextHits(Node node, IndexHits<Relationship> hits) {
		it = hits;
		
		first = !NAME.has(node);
	}

	@Override
	public void close() {
		it.close();
	}

	@Override
	public int size() {
		return -1;
		//return it.size();
	}

	@Override
	public float currentScore() {
		return it.currentScore();
	}

	@Override
	protected Relationship fetchNextOrNull() {
		if (first) {
			first = false;
			if (it.hasNext())
				it.next();
		}

		Relationship r = null;
		while (true) {
			if (!it.hasNext()) return null;
			
			r = it.next();
			if (!r.isType(REF._))
				return r;
		}
	}
}