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

import org.animotron.Properties;
import org.animotron.instruction.AbstractInstruction;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;



/**
 * Instruction 'ml:attribute'.
 * 
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 */
public class ATTRIBUTE extends AbstractInstruction {
	
	private static final ATTRIBUTE INSTANCE = new ATTRIBUTE();
	public static ATTRIBUTE getInstance() { return INSTANCE; }
	
	private ATTRIBUTE() { super("attribute", "ml", "animo/ml"); }
	
	public Node build(Node node, Node value, String ns, String name){
		Relationship relationship = node.createRelationshipTo(value, relationshipType());
		Properties.NAMESPACE.set(relationship, ns);
		Properties.NAME.set(relationship, name);
		return null;
	}

}
