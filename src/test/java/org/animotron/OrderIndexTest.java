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
package org.animotron;

import org.animotron.graph.AnimoGraph;
import org.animotron.graph.GraphOperation;
import org.animotron.graph.index.Order;
import org.apache.lucene.search.Sort;
import org.apache.lucene.search.SortField;
import org.junit.Assert;
import org.junit.Test;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.RelationshipType;
import org.neo4j.graphdb.index.IndexHits;
import org.neo4j.index.lucene.QueryContext;

import java.util.List;

import static org.animotron.graph.AnimoGraph.createNode;
import static org.animotron.graph.AnimoGraph.getROOT;


/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class OrderIndexTest extends ATest {

    @Test
	public void orderedRelationships() throws Throwable {
		if (true) {
            AnimoGraph.execute(new GraphOperation<Void>() {
                @Override
                public Void execute() throws Throwable {
                    createChild(getROOT(), 10000);
                    return null;
                }
            });
		}
		
		System.out.println("reading ...");
		IndexHits<Relationship> q = Order._.queryDown(getROOT());
		try {
			int i = 1;
			for (Relationship r : q ) {

//					System.out.print(q.currentScore() + " ");
//					System.out.println(r.getEndNode().getProperty(NAME+"-P"));
				
				Assert.assertEquals(i, r.getEndNode().getProperty("NAME-P"));
				i++;
			}
		} finally {
			q.close();
		}
		
	}

	private List<Node> createChild(Node parent, int num) {
		
		for (int i = 1; i <= num; i++) {
			Node child = createNode();
			child.setProperty("NAME-P", i);
			Relationship r = parent.createRelationshipTo(child, RT.CHILD);
			Order._.add(r, i);
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
