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
package org.animotron.statement.operator;

import org.animotron.graph.AnimoGraph;
import org.animotron.graph.OrderIndex;
import org.animotron.manipulator.PFlow;
import org.jetlang.channels.Subscribable;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.index.IndexHits;
import org.neo4j.graphdb.traversal.TraversalDescription;
import org.neo4j.kernel.Traversal;

import static org.animotron.graph.RelationshipTypes.REF;
import static org.animotron.graph.RelationshipTypes.RESULT;
import static org.neo4j.graphdb.Direction.OUTGOING;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class Utils {

	public static TraversalDescription td_RESULT = 
		Traversal.description().
			breadthFirst().
			relationships(RESULT, OUTGOING );
			//.evaluator(Evaluators.excludeStartPosition());


	public static Node getByREF(Node node) {
		return node.getSingleRelationship(REF, OUTGOING).getEndNode();
	}
	
	public static boolean results(Node node, PFlow pf) {
		boolean haveSome = false;
		
		Relationship op = pf.getOP();
		
		for (Relationship r : AnimoGraph.getResult(pf.getLastContext(), node)) {
			pf.sendAnswer(op, r);
			
			haveSome = true;
		}
		
//		for (Relationship res : node.getRelationships(OUTGOING)) {
//			
//			if (res.getType().reference().startsWith("RESULT")) {
//				//System.out.println("GET result = "+res);
//				
//				pf.sendAnswer(res);
//				
//				haveSome = true;
//			}
//		}
		return haveSome;
	}
	
	public static boolean haveContext(Node node) {
		
		for (Relationship r : node.getRelationships(OUTGOING)) {
			
			if (r.isType(REF)) {
				//ignore REF
			} else {
				return true;
			}
		}
		
		return false;
	}

	public static boolean haveContext(PFlow pf) {
		
		IndexHits<Relationship> q = OrderIndex.query(pf.getOPNode());
		try {
			while (q.hasNext()) {
				Relationship r = q.next();
				Subscribable<PFlow> onQuestion = pf.getManipulator().onQuestion(r);
				
				if (onQuestion != null) {
					return true;
					
				} else if (r.isType(REF)) {
					//ignore REF
				} else {
					return true;
				}
			}
			
			return false;
		} finally {
			q.close();
		}
	}
}