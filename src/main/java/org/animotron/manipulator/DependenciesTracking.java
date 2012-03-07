/*
 *  Copyright (C) 2011-2012 The Animo Project
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

import static org.neo4j.graphdb.traversal.Evaluation.EXCLUDE_AND_CONTINUE;
import static org.neo4j.graphdb.traversal.Evaluation.EXCLUDE_AND_PRUNE;
import static org.neo4j.graphdb.traversal.Evaluation.INCLUDE_AND_CONTINUE;
import static org.animotron.graph.RelationshipTypes.REV;
import static org.animotron.graph.RelationshipTypes.RESULT;

import org.animotron.graph.index.Order;
import org.animotron.graph.index.State;
import org.animotron.io.Pipe;
import org.animotron.marker.AbstractMarker;
import org.animotron.marker.Marker;
import org.animotron.statement.Statement;
import org.animotron.statement.operator.THE;
import org.neo4j.graphdb.Direction;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Path;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.index.IndexHits;
import org.neo4j.graphdb.traversal.Evaluation;
import org.neo4j.graphdb.traversal.TraversalDescription;
import org.neo4j.kernel.Traversal;

import java.io.IOException;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class DependenciesTracking extends StatementManipulator {
	
	public static DependenciesTracking _ = new DependenciesTracking();
	
	private DependenciesTracking() {};
	
	private static TraversalDescription tdRESULTs = 
			Traversal.description().
				breadthFirst().
				relationships(RESULT).
	            evaluator(new org.neo4j.graphdb.traversal.Evaluator(){
	    			@Override
	    			public Evaluation evaluate(Path path) {

	    				if (path.length() == 0)
	    					return EXCLUDE_AND_CONTINUE;
	    				
	    				System.out.println(path);
	    				
	    				Relationship r = path.lastRelationship();
	    				if (!path.endNode().equals(r.getStartNode()))
	    					return EXCLUDE_AND_PRUNE;
	    				
    					if (path.lastRelationship().isType(THE._))
	    					return INCLUDE_AND_CONTINUE;
	    				
	    				return EXCLUDE_AND_CONTINUE;
	    			}
	    		});

	public Pipe execute(final Controller controller, final Relationship op) throws IOException {
		System.out.println("DependenciesTracking");
		System.out.println(op);
		Node current = THE._.getActualEndNode(op);
		System.out.println(current);
		
		System.out.println("REVs");
		for (Relationship r : current.getRelationships(REV, Direction.OUTGOING)) {
			walker(r);
		}
        return null;
	}
	
	private void walker(Relationship op) {
		System.out.println(op);

		Node n = op.getEndNode();
		
		for (Path path : tdRESULTs.traverse(n)) {
			System.out.println(path);
		}

		IndexHits<Relationship> hits = Order._.queryDown(n);
		for (Relationship r : hits) {
			walker(r);
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