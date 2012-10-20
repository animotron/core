/*
 *  Copyright (C) 2011-2012 The Animo Project
 *  http://animotron.org
 *  	
 *  This file is part of Animotron.
 *  
 *  Animotron is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU Affero General Public License as 
 *  published by the Free Software Foundation, either version 3 of 
 *  the License, or (at your option) any later version.
 *  
 *  Animotron is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Affero General Public License for more details.
 *  
 *  You should have received a copy of 
 *  the GNU Affero General Public License along with Animotron.  
 *  If not, see <http://www.gnu.org/licenses/>.
 */
package org.animotron.statement.operator;

import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;

import static org.neo4j.graphdb.Direction.OUTGOING;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 */
public class AREV extends REF {

	public static final AREV _ = new AREV();

	private AREV() { super("arev"); }

    public Relationship build(Node def, Node rev) {
        return def.createRelationshipTo(rev, this);
    }

    public Relationship set(Node def, Node rev) {
        def.getSingleRelationship(this, OUTGOING).delete();
        return def.createRelationshipTo(rev, this);
    }

    public Node actualNode(Node node) {
        return node.getSingleRelationship(this, OUTGOING).getEndNode();
    }

    public Node actualNode(Relationship relationship) {
        return actualNode(relationship.getEndNode());
    }

    public Node actualEndNode(Relationship r) {
    	Node n = r.getEndNode();

		if (r.isType(REF._) || r.isType(DEF._))
			return actualNode(n);

		return n;
    }

    public Relationship actualRelationship(Relationship r) {
        return r.getEndNode().getSingleRelationship(this, OUTGOING);
    }

    @Override
    public Object reference(Relationship r) {
        return DEF._.reference(r.getStartNode());
    }



}
