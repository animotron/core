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

import java.io.IOException;

import org.animotron.Statement;
import org.animotron.Statements;
import org.animotron.graph.RelationshipTypes;
import org.animotron.io.PipedInput;
import org.animotron.manipulator.Evaluator;
import org.animotron.operator.Query;
import org.animotron.operator.THE;
import org.animotron.operator.relation.IS;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.RelationshipType;
import org.neo4j.graphdb.index.IndexHits;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public abstract class AbstractResultSerializer {
	
	final public void serialize(Relationship r) throws InterruptedException, IOException {
		startDocument();
		build(r);
		endDocument();
	}

	public abstract void start(Statement statement, Relationship r);

	public abstract void end(Statement statement, Relationship r);

	public abstract void startDocument();
	
	public abstract void endDocument();

	protected void build(Relationship r) throws InterruptedException, IOException {
		
		RelationshipType type = r.getType();
		String typeName = type.name();
		
		if (RelationshipTypes.RESULT.name().equals(typeName)) {
			r = getDb().getRelationshipById(
					(Long)r.getProperty(RID.name())
				); 

			type = r.getType();
			typeName = type.name();
		}
		
		Statement s = Statements.relationshipType(typeName);
		
		if (RelationshipTypes.REF.name().equals(typeName) 
			|| typeName.startsWith(THE._.name())) {
			
			s = THE._;
		}
		
		if (s != null) {
			if (s instanceof Query) {
				result(r);

			//workaround IS
			} else if (s instanceof IS) {
				start(s, r);
				end(s, r);

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

	protected boolean result(Relationship r) throws InterruptedException, IOException {
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
		
		if (!found) {
			//UNDERSTAND: calculate current r!
			System.out.println("READER Execute r = "+r);
			PipedInput in = Evaluator._.execute(r);
			
			System.out.println("READER waiting ...");
			for (Object obj : in) {
				System.out.println("READER get from calculator "+obj);
				if (obj instanceof Relationship) {
					build( (Relationship) obj );
				} else {
					System.out.println("UNHANDLED "+obj);
				}
			}
			System.out.println("READER done.");
		}
		
		return found;
	}
}