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

import org.animotron.graph.AnimoGraph;
import org.animotron.graph.AnimoRelationshipType;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.RelationshipType;

/**
 * Operator 'THE'.
 * 
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 */
public class CACHE extends AbstarctOperator {
	
	private static final CACHE INSTANCE = new CACHE();
	public static CACHE getInstance() { return INSTANCE; }
	
	private CACHE() { super("cache", "animo/instance/cache"); }
	
	public RelationshipType relashionshipType(String name){
		return AnimoRelationshipType.get(name(), name);
	}
	
	public Node node(String name) {
		RelationshipType type = relashionshipType(name);
		return AnimoGraph.getNode(AnimoGraph.CACHE, type);
	}
	
	public Node build(String name) {
		return build(AnimoGraph.createNode(), name);
	}
	
	@Override
	public Node build(Node node, String name) {
		RelationshipType type = relashionshipType(name);
		AnimoGraph.CACHE.createRelationshipTo(node, type);
		return node;
	}

}
