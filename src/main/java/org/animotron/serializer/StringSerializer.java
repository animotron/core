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

import java.io.IOException;

import org.animotron.Properties;
import org.animotron.Statement;
import org.animotron.Statements;
import org.animotron.graph.AnimoGraph;
import static org.animotron.graph.RelationshipTypes.RESULT;
import org.animotron.instruction.ml.TEXT;
import org.animotron.interpreter.Calculator;
import org.animotron.operator.THE;
import org.animotron.operator.Query;
import static org.neo4j.graphdb.Direction.OUTGOING;

import org.neo4j.graphdb.Node;
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

	final public void serialize(Relationship r) throws IOException {
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

	protected void build(Relationship r) throws IOException {
		
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
				System.out.print(r);
				System.out.print(" ");
				System.out.println(r.getType().name());

				start(s, r);
				
				IndexHits<Relationship> q = AnimoGraph.getORDER().query(r.getEndNode());
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
		
//			IndexHits<Relationship> q = AnimoGraph.getORDER().query(r.getEndNode());
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

	protected boolean result(Relationship r) throws IOException {
		boolean found = false;
		Node node = r.getEndNode();
		
		System.out.println("searching result from node "+node);
		
		Iterable<Relationship> i = node.getRelationships(RESULT, OUTGOING);
		for ( Relationship n : i ) {
			build( 
				AnimoGraph.graphDb.getRelationshipById(
					(Long)n.getProperty(Properties.RID.name())
				) 
			);
			found = true;
		}
		
		if (found)
			System.out.println("result found");
		else {
			System.out.println("result not found");
			
			for (Object obj : Calculator.eval(r)) {
				if (obj instanceof IOException) {
					throw (IOException)obj;
					
				} else if (obj instanceof Relationship) {
					build((Relationship) obj);
					
				} else
					System.out.println("ignored ["+obj+"]");
			}
		}
		
		return found;
	}
}