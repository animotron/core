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
package org.animotron;

import org.apache.lucene.search.Sort;
import org.apache.lucene.search.SortField;
import org.junit.Assert;
import org.junit.Test;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.RelationshipType;
import org.neo4j.graphdb.Transaction;
import org.neo4j.graphdb.index.IndexHits;
import org.neo4j.index.lucene.QueryContext;

import java.util.List;

import static org.animotron.graph.AnimoGraph.*;


/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class OrderTest extends ATest {

    @Test
	public void orderedRelationships() {
		if (true) {
			Transaction tx = beginTx();
			try {
				createChild(getROOT(), 10000);
				tx.success();
			} finally {
				finishTx(tx);
			}
		}
		
		System.out.println("reading ...");
		IndexHits<Relationship> q = getORDER().query(getROOT());
		try {
			int i = 1;
			for (Relationship r : q ) {

//					System.out.print(q.currentScore() + " ");
//					System.out.println(r.getEndNode().getProperty(ORDER+"-P"));
				
				Assert.assertEquals(i, r.getEndNode().getProperty("ORDER-P"));
				i++;
			}
		} finally {
			q.close();
		}
		
	}

	private List<Node> createChild(Node parent, int num) {
		
		for (int i = 1; i <= num; i++) {
			Node child = createNode();
			child.setProperty("ORDER-P", i);
			Relationship r = parent.createRelationshipTo(child, RT.CHILD);
			getORDER().add(r, i);
		}

		return null;
	}
	
	public QueryContext sort( String key, String... additionalKeys ) {
		QueryContext q = new QueryContext( "*" );
		
        SortField firstSortField = new SortField( key, SortField.LONG );
        if ( additionalKeys.length == 0 )
        {
            return q.sort( new Sort( firstSortField ) );
        }
        
        SortField[] sortFields = new SortField[1+additionalKeys.length];
        sortFields[0] = firstSortField;
        for ( int i = 0; i < additionalKeys.length; i++ )
        {
            sortFields[1+i] = new SortField( additionalKeys[i], SortField.LONG );
        }
        return q.sort( new Sort( sortFields ) );
    }

	enum RT implements RelationshipType {
		CHILD
	}
}
