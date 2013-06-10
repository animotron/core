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
package org.animotron.manipulator;

import javolution.util.FastSet;

import org.animotron.graph.AnimoGraph;
import org.animotron.graph.GraphOperation;
import org.animotron.graph.index.Order;
import org.animotron.graph.index.State;
import org.animotron.graph.serializer.Serializer;
import org.animotron.io.Pipe;
import org.animotron.marker.AbstractMarker;
import org.animotron.marker.Marker;
import org.animotron.statement.Statement;
import org.animotron.statement.operator.REF;
import org.animotron.statement.operator.Utils;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Path;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.index.IndexHits;

import static org.animotron.graph.RelationshipTypes.RESULT;
import static org.neo4j.graphdb.Direction.INCOMING;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class DependenciesTracking extends StatementManipulator {
	
	public static DependenciesTracking _ = new DependenciesTracking();
	
	private DependenciesTracking() {};
	
	public Pipe execute(final Controller controller, final Relationship op) throws Throwable {
//		System.out.println("DependenciesTracking");
		final Node current = op.getEndNode();
//		System.out.println(current);
		AnimoGraph.execute(new GraphOperation<Void>() {
            @Override
            public Void execute() throws Throwable {
                walker(current);

//                Node n = null;
//                for (Relationship r : current.getRelationships(SHIFT, INCOMING)) {
//                    n = r.getStartNode();
//                    walker(n);
//                }
                return null;
            }
        });
        return null;
	}
	
	private void walker(Node node) throws Throwable {
		FastSet<Node> visited = FastSet.newInstance();
		FastSet<Node> set = FastSet.newInstance();
		FastSet<Node> next = FastSet.newInstance();
		set.add(node);

		try {
			while (!set.isEmpty()) {
				for (FastSet.Record record = set.head(), end = set.tail(); (record = record.getNext()) != end;) {
					Node n = set.valueOf(record);
					visited.add(n);
					
					for (Relationship r : n.getRelationships(REF._, INCOMING)) {
						for (Path path : Utils.EXPS.traverse(r.getStartNode())) {
							//System.out.println(fs);
							Serializer.drop(path.lastRelationship());
						}
					}
			
					for (Relationship r : n.getRelationships(RESULT, INCOMING)) {
						for (Path path : Utils.EXPS.traverse(r.getStartNode())) {
							//System.out.println(fs);
							Serializer.drop(path.lastRelationship());
						}
						r.delete();
					}
					
					IndexHits<Relationship> hits = Order._.queryDown(n);
					for (Relationship r : hits) {
						final Node nn = r.getEndNode();
						if (!visited.contains(nn))
							next.add(r.getEndNode());
					}
				}
				set.clear();
				final FastSet<Node> tmp = set;
				set = next;
				next = tmp;
			}			
		} finally {
			FastSet.recycle(set);
			FastSet.recycle(next);
			FastSet.recycle(visited);
		}
	}
	
	@Override
	public boolean canGo(Statement statement) {
		return false;
	}

	@Override
	public OnQuestion onQuestion(Statement statement, Relationship op) {
		return null;
	}

	@Override
	public Marker marker() {
		return DependenciesTrackingMarker._;
	}
	
	private static class DependenciesTrackingMarker extends AbstractMarker {
		
		private static final Marker _ = new DependenciesTrackingMarker();
		private DependenciesTrackingMarker() {super(State.PREPARE);}

		@Override
		public Manipulator manipulator() {
			return DependenciesTracking._;
		}
		
	}
}