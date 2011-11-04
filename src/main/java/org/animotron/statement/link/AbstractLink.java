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
package org.animotron.statement.link;

import org.animotron.exception.AnimoException;
import org.animotron.statement.AbstractStatement;
import org.animotron.statement.operator.THE;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;

import static org.animotron.graph.AnimoGraph.createNode;
import static org.animotron.graph.RelationshipTypes.REF;
import static org.neo4j.graphdb.Direction.OUTGOING;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public abstract class AbstractLink extends AbstractStatement {

	protected AbstractLink(String name) { super(name); }

    @Override
    protected Node createChild(Object reference, boolean ready, boolean ignoreNotFound) throws AnimoException {
        return createNode();
    }

    @Override
    public Object reference(Relationship r) {
        Node node = r.getEndNode();
        if (node.hasRelationship(LINK._, OUTGOING)) {
            return THE._.reference(node.getRelationships(LINK._, OUTGOING).iterator().next());
        }
        return super.reference(r);
    }

}