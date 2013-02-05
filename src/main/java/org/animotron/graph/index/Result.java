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
package org.animotron.graph.index;

import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.RelationshipType;
import org.neo4j.graphdb.index.IndexHits;
import org.neo4j.graphdb.index.IndexManager;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class Result extends AbstractRelationshipIndex {
	
    public static Result _ = new Result("result");
    
    private Result(String name){super(name);}
    
	public void init(IndexManager index) {
        //init(index.forRelationships(name, BerkeleyDbIndexImplementation.DEFAULT_CONFIG));
        init(index.forRelationships(name));
	}

//    public ResultHits getHits(byte[] expression) {
//        return new ResultHits(getHits(MessageDigester.byteArrayToHex(expression)));
//    }

    public Relationship getIfExist(Node sNode, Relationship result, RelationshipType type) {
        IndexHits<Relationship> hits = getHits(null, sNode, result.getEndNode());
        try {
        	for (Relationship r : hits) {
        		if (r.isType(type)) {
        			if (r.getProperty("RID").equals(result.getId()))
        				return r;
        		}
        	}
        } finally {
        	hits.close();
        }
        return null;
    }
}