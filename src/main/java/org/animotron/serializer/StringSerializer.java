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
package org.animotron.serializer;

import static org.animotron.Properties.RID;
import static org.animotron.graph.AnimoGraph.getDb;
import static org.animotron.graph.AnimoGraph.getORDER;
import static org.neo4j.graphdb.Direction.OUTGOING;

import org.animotron.Statement;
import org.animotron.Statements;
import org.animotron.graph.RelationshipTypes;
import org.animotron.instruction.ml.TEXT;
import org.animotron.operator.Query;
import org.animotron.operator.THE;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.RelationshipType;
import org.neo4j.graphdb.index.IndexHits;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class StringSerializer {
	
	private StringBuilder builder;
	private String result;
	
	public String getString() {
		return result;
	}

	final public void serialize(Relationship r) {
		startDocument();
		build(r);
		endDocument();
	}

	/* (non-Javadoc)
	 * @see org.animotron.graph.GraphHandler#start(org.animotron.Statement, org.neo4j.graphdb.Relationship)
	 */
//	@Override
	public void start(Statement statement, Relationship r) {
		//System.out.println(r);
		if (statement instanceof TEXT) {
			System.out.println("TEXT found");
			builder.append(statement.value(r));
		} else {
			//build(r);
		}
	}

	/* (non-Javadoc)
	 * @see org.animotron.graph.GraphHandler#end(org.animotron.Statement, org.neo4j.graphdb.Relationship)
	 */
//	@Override
	public void end(Statement statement, Relationship r) {
	}

	/* (non-Javadoc)
	 * @see org.animotron.graph.GraphHandler#startDocument()
	 */
//	@Override
	public void startDocument() {
		builder = new StringBuilder(1024);
		result = null;
	}

	/* (non-Javadoc)
	 * @see org.animotron.graph.GraphHandler#endDocument()
	 */
//	@Override
	public void endDocument() {
		result = builder.toString();
		builder = null;
	}

	protected void build(Relationship r) {
		
		RelationshipType type = r.getType();
		String typeName = type.name();
		
		Statement s = Statements.relationshipType(typeName);
		
		if (typeName.startsWith(THE._.name())) {
			s = THE._;
		}
		
		if (s != null) {
			if (s instanceof Query) {
				result(r);

			} else {
				start(s, r);
				
				IndexHits<Relationship> q = getORDER().query(r.getEndNode());
				try {
					for (Relationship i : q) {
						build(i);
					}
				} finally {
					q.close();
				}
			
				end(s, r);
			}
		}
		
		System.out.print(r);
		System.out.print(" ");
		System.out.println(r.getType().name());
//			IndexHits<Relationship> q = getORDER().query(r.getEndNode());
//			try {
//				for (Relationship i : q) {
//					build(i);
//				}
//			} finally {
//				q.close();
//			}
//		}
//		end(statement, r);
	}

	protected boolean result(Relationship r) {
		boolean found = false;
		Iterable<Relationship> i = r.getEndNode().getRelationships(RelationshipTypes.RESULT, OUTGOING);
		for ( Relationship n : i ) {
			build( 
				getDb().getRelationshipById(
					(Long)n.getProperty(RID.name())
				) 
			);
			found = true;
		}
		
		return found;
	}
}