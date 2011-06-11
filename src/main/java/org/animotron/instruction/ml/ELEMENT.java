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
import org.animotron.graph.AnimoGraph;
import org.animotron.instruction.AbstractInstruction;
import org.animotron.operator.Cachable;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;

/**
 * Instruction 'ml:element'.
 * 
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 */
public class ELEMENT extends AbstractInstruction implements Cachable {
	
	public static final ELEMENT _ = new ELEMENT();
	
	private ELEMENT() { super("element", ML._); }
	
	@Override
	public Node build(Node parent, String prefix, String ns, String name, Node value, int order) {
		Node child = AnimoGraph.createNode();
		Relationship r = parent.createRelationshipTo(child, relationshipType());
		AnimoGraph.order(r, order);
		Properties.PREFIX.set(child, prefix);
		Properties.NAMESPACE.set(child, ns);
		Properties.NAME.set(child, name);
		return child;
	}
	
	@Override
	public String name(Relationship r){
		return Properties.NAME.get(r.getEndNode());
	}
	
	@Override
	public String namespace(Relationship r){
		Node node = r.getEndNode();
		return Properties.NAMESPACE.has(node) ? Properties.NAMESPACE.get(node) : null;
	}
	
	@Override
	public String prefix(Relationship r){
		Node node = r.getEndNode();
		return Properties.PREFIX.has(node) ? Properties.PREFIX.get(node) : null;
	}
}