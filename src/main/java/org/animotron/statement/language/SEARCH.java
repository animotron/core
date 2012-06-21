/*
 *  Copyright (C) 2011-2012 The Animo Project
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
package org.animotron.statement.language;

import javolution.util.FastSet;
import org.animotron.manipulator.PFlow;
import org.animotron.statement.operator.Operator;
import org.animotron.statement.operator.Predicate;
import org.animotron.statement.operator.Utils;
import org.animotron.statement.string.STRING;
import org.neo4j.graphdb.Path;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.index.IndexHits;

import java.io.IOException;
import java.util.Set;

/**
 * 'SEARCH' instruction.
 *
 *  @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 */
public class SEARCH extends Operator implements Predicate {

	public static final SEARCH _ = new SEARCH();
	
	private SEARCH() { super("search"); }


	@Override
	public boolean filter(PFlow pf, Relationship op, Relationship ref) throws InterruptedException, IOException {
		// TODO Auto-generated method stub
		return false;
	}


	@Override
	public Set<Relationship> getExpected(PFlow pf, Relationship op) throws InterruptedException, IOException {
		Set<Relationship> set = new FastSet<Relationship>();
		
		StringBuilder sb = STRING._.eval(pf, op.getEndNode());
		if (sb != null && sb.length() > 0) {
			IndexHits<Relationship> hits = LABEL._.search(sb.toString());
			try {
				for (Relationship r : hits) {
					for (Path path : Utils.THES.traverse(r.getStartNode())) {
						set.add(path.lastRelationship());
					}
				}
			} finally {
				hits.close();
			}
		}
		return set;
	}
}