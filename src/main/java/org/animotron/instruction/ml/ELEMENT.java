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
package org.animotron.instruction.ml;

import java.io.IOException;

import org.animotron.annotation.Namespace;
import org.animotron.instruction.Instruction;
import org.animotron.io.PipedOutputObjectStream;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.RelationshipType;

/**
 * Instruction 'ml:element'.
 * 
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 */
@Namespace(prefix = "ml", uri = "animo/ml")
public class ELEMENT implements Instruction {
	
	private static class SingletonHolder { 
		public static final ELEMENT INSTANCE = new ELEMENT();

		public static final RelationshipType relationshipType = new RelationshipType() {
			@Override
			public String name() {
				return "ML:ELEMENT";
			}
		};  
	}
	
	public static ELEMENT getInstance() {
		return SingletonHolder.INSTANCE;
	}
	
	private ELEMENT() {}

	@Override
	public String name() {
		return "element";
	}

	@Override
	public RelationshipType relationshipType() {
		return SingletonHolder.relationshipType;
	}

	@Override
	public void eval(Relationship op, PipedOutputObjectStream out, boolean isLast) throws IOException {
		//TODO: code
	}
}
