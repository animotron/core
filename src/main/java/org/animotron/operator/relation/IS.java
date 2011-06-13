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
package org.animotron.operator.relation;

import static org.neo4j.graphdb.Direction.INCOMING;

import java.io.IOException;

import org.animotron.graph.RelationshipTypes;
import org.animotron.io.PipedOutputObjectStream;
import org.animotron.operator.Prepare;
import org.animotron.operator.Relation;
import org.neo4j.graphdb.Relationship;

/**
 * Operator 'IS'.
 * 
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 */
public class IS extends Relation implements Prepare {
	
	public static final IS _ = new IS();
	
	private IS() { super("is", "animo/relation/is"); }

	@Override
	public void prepare(Relationship op, PipedOutputObjectStream out,
			boolean isLast) throws IOException {
		
		op.getStartNode().getSingleRelationship(RelationshipTypes.TOP, INCOMING).delete();
		
	}
	
}