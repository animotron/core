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

import org.animotron.instruction.AbstractInstruction;
import org.animotron.operator.Result;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;

import static org.animotron.Properties.*;
import static org.animotron.graph.AnimoGraph.order;


/**
 * Instruction 'ml:attribute'.
 * 
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 */
public class ATTRIBUTE extends AbstractInstruction implements Result {
	
	public static final ATTRIBUTE _ = new ATTRIBUTE();
	
	private ATTRIBUTE() { super("attribute", ML._); }
	
	@Override
	public Relationship build(Node parent, String prefix, String ns, String name, Node value, int order, boolean ignoreNotFound) {
		Relationship r = parent.createRelationshipTo(value, relationshipType());
		order(r, order);
		PREFIX.set(r, prefix);
		NAMESPACE.set(r, ns);
		NAME.set(r, name);
		return r;
	}
	
	@Override
	public String prefix(Relationship r){
		return PREFIX.has(r) ? PREFIX.get(r) : null;
	}
	
	@Override
	public String name(Relationship r){
		return NAME.get(r);
	}
	
	@Override
	public String namespace(Relationship r){
		return NAMESPACE.has(r) ? NAMESPACE.get(r) : null;
	}
	
	@Override
	public String value(Relationship r){
		return VALUE.get(r.getEndNode());
	}
}