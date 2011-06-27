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
package org.animotron.manipulator;

import static org.animotron.graph.AnimoGraph.getTOP;
import static org.neo4j.graphdb.Direction.OUTGOING;

import java.io.IOException;
import java.util.Iterator;
import java.util.List;

import javolution.util.FastList;

import org.animotron.exception.EBuilderTerminated;
import org.animotron.graph.RelationshipTypes;
import org.animotron.operator.THE;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Path;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.traversal.Evaluators;
import org.neo4j.graphdb.traversal.TraversalDescription;
import org.neo4j.helpers.Predicate;
import org.neo4j.kernel.Traversal;
import org.neo4j.kernel.Uniqueness;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class Manipulators {
	
	private static Manipulators _ = new Manipulators();
	
	private Manipulators() {}
	
	//TODO: Implement manipulators/listeners/broadcasters loader
	
	public class Catcher {
		
		List<Node> creative = new FastList<Node>();
		List<Node> destructive = new FastList<Node>();
		
		public Catcher() {}
		
		public void creative(Relationship r) throws EBuilderTerminated {
			creative.add(r.getEndNode());
		}
		
		public void renew(Relationship r) throws EBuilderTerminated {
			for (Relationship i : r.getEndNode().getRelationships(OUTGOING)) {
				destructive(i);
			}
			Node node = r.getEndNode();
			getTOP().createRelationshipTo(node, RelationshipTypes.TOP);
			
			creative(r);
		}

		private void destructive(Relationship r) throws EBuilderTerminated {
			creative.add(r.getEndNode());

			r.delete();
		}
		

		public void push() throws EBuilderTerminated {
			creative();
			destructive();
		}
		
		private void creative() throws EBuilderTerminated {
			
			try {
				for (Node n : creative) {
					Preparator._.execute(n);
					Iterator<Path> it = TD.traverse(n).iterator();
					while (it.hasNext()) {
						Preparator._.execute(it.next().relationships().iterator().next());
					}
				}
			} catch (InterruptedException e) {
				throw new EBuilderTerminated(e);
			} catch (IOException e) {
				throw new EBuilderTerminated(e);
			}
		}
		
		private void destructive() throws EBuilderTerminated {
			try {
				for (Node n : destructive) {
					GC._.execute(n);
				}
			} catch (InterruptedException e) {
				throw new EBuilderTerminated(e);
			} catch (IOException e) {
				throw new EBuilderTerminated(e);
			}
		}
	}
	
	@SuppressWarnings("deprecation")
	private static TraversalDescription TD = 
	     Traversal.description()
		.depthFirst()
		.uniqueness(Uniqueness.RELATIONSHIP_PATH)
		.evaluator(Evaluators.atDepth(2))
		.filter(new Predicate<Path> (){
				@Override
				public boolean accept(Path path) {
					if (THE._.NODE().equals(path.startNode()))
						return true;
					Relationship r = path.lastRelationship();
					if (r != null && RelationshipTypes.REF.equals(r.getType()))
						return true;
					return false;
				}
			});

	public static Catcher getCatcher() {
		return _.new Catcher();
	}

}