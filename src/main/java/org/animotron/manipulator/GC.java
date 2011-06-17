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
import static org.neo4j.graphdb.Direction.OUTGOING;

import java.io.IOException;

import org.animotron.graph.RelationshipTypes;
import org.animotron.io.PipedInputObjectStream;
import org.animotron.io.PipedOutputObjectStream;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.PropertyContainer;
import org.neo4j.graphdb.Relationship;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class GC extends GraphListener implements SimpleManipulator {
	
	protected static GC _ = new GC();
	
	private GC() {
		super(RelationshipTypes.GC, Destructive._);
	}
	
	@Override
	public void push(final Relationship op, Catcher catcher, PipedOutputObjectStream out) throws ExceptionBuilderTerminate {
		
		System.out.println("GC the relationship " + op);
		
		for (Relationship r : op.getEndNode().getRelationships(INCOMING)) { 
			if (!r.equals(op)) {
				catcher.add(Preparator._.walk(r, out));
			}
		}
		
		for (Relationship r : op.getEndNode().getRelationships(OUTGOING)) {
			Node node = r.getEndNode();
			if (!node.hasRelationship(INCOMING)) {
				catcher.add(walk(node, out));
				Destructive._.push(r, catcher);
			} else {
				r.delete();
			}
		}
		
	}

	@Override
	public void go(Relationship op, PipedOutputObjectStream ot, boolean isLast) throws IOException {
		op.getStartNode().delete();
	}

	@Override
	public UnconditionalWalker walk(PropertyContainer op, PipedOutputObjectStream out) {
		return new UnconditionalWalker (this, op, out);
	}
	
	@Override
	public boolean isPiped() {
		return true;
	}

	@Override
	public PipedInputObjectStream execute(PropertyContainer op) throws IOException {
		return Executor.execute(this, op);
	}

	@Override
	public void execute(PropertyContainer op, PipedOutputObjectStream out) {
		Executor.execute(this, op, out);
	}
	
}
