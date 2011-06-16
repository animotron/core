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

import java.io.IOException;

import org.animotron.Statement;
import org.animotron.graph.RelationshipTypes;
import org.animotron.io.PipedInputObjectStream;
import org.animotron.io.PipedOutputObjectStream;
import org.neo4j.graphdb.PropertyContainer;
import org.neo4j.graphdb.Relationship;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class GC extends GraphListener implements Manipulator {
	
	protected static GC _ = new GC();
	
	private GC() {
		super(RelationshipTypes.GC, Destructive._);
	}
	
	@Override
	public void push(final Relationship op, PipedOutputObjectStream out) {
		System.out.println("GC the relationship " + op);
	}

	@Override
	public boolean canGo(Statement statement) {
		// TODO Auto-generated method stub
		return true;
	}

	@Override
	public void go(Statement statement, Relationship op, PipedOutputObjectStream ot, boolean isLast) throws IOException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public Walker<GC> walk(PropertyContainer op, PipedOutputObjectStream out) {
		return new Walker<GC>(this, op, out);
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
