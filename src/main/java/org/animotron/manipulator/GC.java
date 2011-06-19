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

import static org.neo4j.graphdb.Direction.INCOMING;

import java.io.IOException;

import org.animotron.exception.ExceptionBuilderTerminate;
import org.animotron.graph.RelationshipTypes;
import org.animotron.io.PipedOutputObjectStream;
import org.animotron.marker.AbstractMarker;
import org.animotron.marker.Marker;
import org.animotron.walker.Walker;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.PropertyContainer;
import org.neo4j.graphdb.Relationship;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class GC extends AbstractSimpleManipulator {
	
	public static GC _ = new GC();
	private GC() {Destructive._.register(new Garbage());}

	@Override
	public void go(Relationship op, PipedOutputObjectStream ot, Catcher catcher, boolean isLast) throws IOException {
		Node node = op.getEndNode();
		op.getStartNode().delete();
		op.delete();
		if (!node.hasRelationship(INCOMING)) {
			if (isLast) {
				node.delete();
			} else {
				catcher.add(markWalk(node, ot));
			}
		}


	}

	@Override
	public Walker markWalk(PropertyContainer op, PipedOutputObjectStream out) {
		return walk(op, out, GCMarker._);
	}
	
	public class Garbage extends AbstaractGraphListener {
		
		@Override
		public void push(final Relationship op, Catcher catcher, PipedOutputObjectStream out) throws ExceptionBuilderTerminate {
			
			System.out.println("GC the relationship " + op);
			
			Node node = op.getEndNode();
			op.delete();
			if (!node.hasRelationship(INCOMING)) {
				catcher.add(markWalk(node, out));
			}
			
		}
		
	}
	
	private static class GCMarker extends AbstractMarker {

		private static final Marker _ = new GCMarker(); 
		private GCMarker() {super(RelationshipTypes.GC);}

		@Override
		public Manipulator manipulator() {
			return GC._;
		}
		
	}
}
