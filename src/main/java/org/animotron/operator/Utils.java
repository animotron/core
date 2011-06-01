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
package org.animotron.operator;

import java.io.IOException;

import org.animotron.graph.RelationshipTypes;
import org.animotron.io.PipedOutputObjectStream;
import org.neo4j.graphdb.Direction;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.traversal.TraversalDescription;
import org.neo4j.kernel.Traversal;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class Utils {

	public static TraversalDescription td_RESULT = 
		Traversal.description().
			breadthFirst().
			relationships(RelationshipTypes.RESULT, Direction.OUTGOING );
			//.evaluator(Evaluators.excludeStartPosition());


	public static Node getByREF(Node node) {
		return 
			node.getSingleRelationship(
				RelationshipTypes.REF, 
				Direction.OUTGOING
			).getEndNode();
	}
	
	public static boolean results(Node node, PipedOutputObjectStream out) throws IOException {
		
		boolean haveSome = false;
		for (Relationship res : node.getRelationships(Direction.OUTGOING)) {
			
			if (res.getType().name().startsWith("RESULT:")) {
				System.out.println("GET result = "+res);
				
				out.write(res);
				
				haveSome = true;
			}
		}
		return haveSome;
	}
}
