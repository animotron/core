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
package org.animotron.operator;

import java.io.IOException;

import org.animotron.graph.AnimoRelationshipType;
import org.animotron.graph.RelationshipTypes;
import org.animotron.interpreter.Calculator;
import org.animotron.io.PipedInputObjectStream;
import org.animotron.io.PipedOutputObjectStream;
import org.neo4j.graphdb.Direction;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;

/**
 * Operation 'AN'. Direct reference to 'the' instance.
 * 
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 */
public class AN extends AbstractOperator implements Evaluable{
	
	private static final AN INSTANCE = new AN();
	public static AN getInstance() { return INSTANCE; }
	
	private AN() { super("an", "animo/reference"); }
	
	@Override
	public void eval(Relationship op, PipedOutputObjectStream out, boolean isLast) throws IOException {
		PipedInputObjectStream in = new PipedInputObjectStream();

		if (!isLast)
			Calculator.eval(op, new PipedOutputObjectStream(in));
		
		//go to 'THE' node
		for (Relationship r : op.getEndNode().getRelationships(RelationshipTypes.REF, Direction.OUTGOING)) {
			
			//get 'THE' relation
			Node node = r.getEndNode();
			String name = (String) node.getProperty("NAME");
			for (Relationship t : node.getRelationships(new AnimoRelationshipType(name), Direction.INCOMING)) {
				out.write(t);
			}
		}
		//XXX: what do to with that?
//		Object n; 
//		while ((n = in.read()) != null) {
//			out.write(n);
//		} 
	}

}
