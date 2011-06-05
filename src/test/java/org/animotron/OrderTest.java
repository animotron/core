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

import java.util.List;

import org.junit.Test;
import org.neo4j.graphdb.GraphDatabaseService;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.RelationshipType;
import org.neo4j.graphdb.Transaction;
import org.neo4j.graphdb.index.IndexHits;
import org.neo4j.graphdb.index.IndexManager;
import org.neo4j.graphdb.index.RelationshipIndex;
import org.neo4j.index.lucene.QueryContext;
import org.neo4j.kernel.EmbeddedGraphDatabase;


/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class OrderTest {
	
	private static final String ORDER = "order"; 
	
	GraphDatabaseService graphDb;
	RelationshipIndex order;
	
	@Test
	public void orderedRelationships() {
		Node ROOT;
		
		graphDb = new EmbeddedGraphDatabase("data-tests");
		
		IndexManager index = graphDb.index();
		
		Transaction tx;
		
		if (false) {
			tx = graphDb.beginTx();
			try {
				order = index.forRelationships( ORDER );
		
				ROOT = graphDb.getReferenceNode();
				
				List<Node> children = createChild(ROOT, 25);
				
				tx.success();
			} finally {
				tx.finish();
			}
		}
		
		tx = graphDb.beginTx();
		try {
			ROOT = graphDb.getReferenceNode();
			order = index.forRelationships( ORDER );

			System.out.println("reading ...");
			IndexHits<Relationship> q = order.query(ORDER, new QueryContext( "*" ).sort( ORDER ), ROOT, null);
			try {
				for (Relationship r : q ) {
					
					System.out.print(q.currentScore() + " ");
					System.out.println(r.getEndNode().getProperty(ORDER+"-P"));
				}
			} finally {
				q.close();
			}
		} finally {
			tx.finish();
		}
	}

	private List<Node> createChild(Node parent, int num) {
		
		for (int i = 1; i <= num; i++) {
			Node child = graphDb.createNode();
			child.setProperty(ORDER+"-P", i);
			
			Relationship r = parent.createRelationshipTo(child, RT.CHILD);
			
			order.add(r, ORDER, i);
		}

		return null;
	}

	enum RT implements RelationshipType {
		CHILD
	}
}
