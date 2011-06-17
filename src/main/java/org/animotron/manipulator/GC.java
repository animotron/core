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

import org.animotron.graph.RelationshipTypes;
import org.animotron.io.PipedOutputObjectStream;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class GC extends AbstractSimpleManipulator implements GraphListener {
	
	protected static GC _ = new GC();
	
	private GC() {super(RelationshipTypes.GC);}
	
	@Override
	public void push(final Relationship op, Catcher catcher, PipedOutputObjectStream out) throws ExceptionBuilderTerminate {
		
		System.out.println("GC the relationship " + op);
		
		Node node = op.getEndNode();
		op.delete();
		if (!node.hasRelationship(INCOMING)) {
			catcher.add(walk(node, out));
		}
		
	}

	@Override
	public void go(Relationship op, PipedOutputObjectStream ot, boolean isLast) throws IOException {
		Node node = op.getEndNode();
		op.getStartNode().delete();
		op.delete();
		if (!node.hasRelationship(INCOMING)) {
			if (isLast) {
				node.delete();
			} else {
				execute(node, ot);
			}
		}
	}

	@Override
	public void push(Relationship op, Catcher catcher) throws ExceptionBuilderTerminate {
		push(op, catcher, null);
	}

	
}
