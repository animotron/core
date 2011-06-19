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
package org.animotron.walker;

import java.io.IOException;

import org.animotron.Catcher;
import org.animotron.Statement;
import org.animotron.Statements;
import org.animotron.graph.RelationshipTypes;
import org.animotron.instruction.Instruction;
import org.animotron.io.PipedInputObjectStream;
import org.animotron.io.PipedOutputObjectStream;
import org.animotron.manipulator.StatementManipulator;
import org.animotron.marker.Marker;
import org.neo4j.graphdb.PropertyContainer;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.RelationshipType;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * 
 */
public class ConditionalWalker extends Walker {

	public ConditionalWalker(StatementManipulator m, PropertyContainer op, PipedOutputObjectStream out, Marker marker) {
		super(m, op, out, marker);
	}

	@Override
	protected void go(Relationship op, PipedOutputObjectStream ot, Catcher catcher, boolean isLast) throws IOException {

		StatementManipulator m = (StatementManipulator) getManipulator();
		
		try {
			RelationshipType type = op.getType();

			//System.out.println(type.name());

			Statement s = Statements.relationshipType(type);

			if (m.canGo(s)) {

				PipedInputObjectStream in = null;
				PipedOutputObjectStream out = ot;

				if (m.isPiped()) {
					in = new PipedInputObjectStream();
					out = new PipedOutputObjectStream(in);
				}

				m.go(s, op, out, catcher, isLast);

				if (in != null) {
					for (Object n : in) {
						ot.write(n);
					}
				}
				
			} else if (s instanceof Instruction) {
				//bypass instructions
				ot.write(op);
				
			//XXX:find better solution
			} else if (type.equals(RelationshipTypes.REF)) {
				//ignore
				
			} else {
				System.out.println("Not evaled " + op);
				//ot.write(r);
			}

		} catch (IOException e) {
			e.printStackTrace();
			ot.write(e);
		}

		ot.close();
	}

}
