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

import org.animotron.graph.index.Order;
import org.animotron.graph.index.State;
import org.animotron.io.PipedInput;
import org.animotron.marker.AbstractMarker;
import org.animotron.marker.Marker;
import org.animotron.statement.Statement;
import org.animotron.statement.Statements;
import org.animotron.statement.operator.AN;
import org.animotron.statement.operator.Prepare;
import org.animotron.statement.operator.REF;
import org.animotron.statement.operator.THE;
import org.animotron.statement.relation.USE;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.index.IndexHits;

import java.io.IOException;

import static org.neo4j.graphdb.Direction.*;

/**
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class Preparator extends StatementManipulator {
	
	public static Preparator _ = new Preparator();
	
	private Preparator() {};
	
	public PipedInput<QCAVector> execute(Node op) throws IOException {
        for (Relationship r : op.getRelationships(INCOMING)) {
            if (r.isType(AN._) || r.isType(USE._) || r.isType(REF._)) {
			    //XXX: rewrite super.execute(r);
            }
		}
        //System.out.println("Preparator "+op);
        IndexHits<Relationship> hits = Order.queryDown(op);
        try {
        	for (Relationship r : hits) {
        		Statement s = Statements.relationshipType(r);
        		if (s instanceof AN) {
        			try {
        				Relationship ref = r.getEndNode().getSingleRelationship(REF._, OUTGOING);
        				String name = (String) THE._.reference(ref);
        				if (name != null) {
	                        s = Statements.name(name);
	        			    
	                        if (s instanceof Prepare) {
	                            super.execute(new QCAVector(r), onQuestion(s, r), true);
							}
        				}
        			} catch (Exception e) {
        				//e.printStackTrace();
					}
				} else if (s instanceof Prepare) {
    			    super.execute(r);
				}
        	}
        } finally {
        	hits.close();
        }
        return null;
	}
	
	@Override
	public boolean canGo(Statement statement) {
		return statement instanceof Prepare;
	}

	@Override
	public OnQuestion onQuestion(Statement statement, Relationship op) {
		return ((Prepare) statement).onPrepareQuestion();
	}

	@Override
	public Marker marker() {
		return PrepareMarker._;
	}
	
	private static class PrepareMarker extends AbstractMarker {
		
		private static final Marker _ = new PrepareMarker();
		private PrepareMarker() {super(State.PREPARE);}

		@Override
		public Manipulator manipulator() {
			return Preparator._;
		}
		
	}

}