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

import org.animotron.utils.MessageDigester;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.RelationshipType;
import org.neo4j.graphdb.index.IndexHits;
import org.neo4j.graphdb.index.IndexManager;
import org.neo4j.graphdb.index.RelationshipIndex;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class Result {
	
	public static final String NAME = "result";
	
	private static RelationshipIndex INDEX;

	public static void init(IndexManager indexManager) {
        //INDEX = indexManager.forRelationships(NAME, BerkeleyDbIndexImplementation.DEFAULT_CONFIG);
        INDEX = indexManager.forRelationships(NAME);
	}

    public static void add(Relationship r, byte[] value) {
        INDEX.add(r, NAME, MessageDigester.byteArrayToHex(value));
    }
	
//    public static ResultHits get(byte[] value) {
//    	IndexHits<Relationship> hits = INDEX.get(NAME, MessageDigester.byteArrayToHex(value));
//        return new ResultHits( hits );
//    }

    public static ResultHits get(byte[] value, Relationship op) {
        return new ResultHits(
    		op,
    		INDEX.get(NAME, MessageDigester.byteArrayToHex(value), op.getEndNode(), null)
		);
    }

    public static Relationship getIfExist(Node sNode, Relationship result, RelationshipType type) {
        IndexHits<Relationship> hits = INDEX.get(NAME, null, sNode, result.getEndNode());
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