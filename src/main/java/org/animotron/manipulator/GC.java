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

import org.animotron.graph.AnimoGraph;
import org.animotron.graph.GraphOperation;
import org.animotron.graph.index.Cache;
import org.animotron.graph.index.State;
import org.animotron.marker.AbstractMarker;
import org.animotron.marker.Marker;
import org.jetlang.channels.Subscribable;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;

import static org.neo4j.graphdb.Direction.INCOMING;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class GC extends Manipulator {
	
	public static GC _ = new GC();
	
	private GC () {}

	private OnQuestion question = new OnQuestion() {
		
		private void garbage(PFlow pf) {
			Node node = pf.getOPNode();
			if (!node.hasRelationship(INCOMING)) {
				super.onMessage(pf);
                for (Relationship r : node.getRelationships()) {
                    r.delete();
                }
                Cache.removeNode(node);
				node.delete();
			}
		}
		
		@Override
		public void onMessage(final PFlow pf) {
			AnimoGraph.execute(
				new GraphOperation<Void>(){
					@Override
					public Void execute() {
						garbage(pf);
						return null;
					}
					
				}
			);

		}

	};

	@Override
	public Subscribable<PFlow> onQuestion(Relationship op) {
		return question;
	}

	@Override
	public Marker marker() {
		return GCMarker._;
	}
	
	private static class GCMarker extends AbstractMarker {

		private static final Marker _ = new GCMarker(); 
		private GCMarker() {super(State.GC);}

		@Override
		public Manipulator manipulator() {
			return GC._;
		}
		
	}
}
