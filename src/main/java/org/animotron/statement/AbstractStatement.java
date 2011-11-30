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
package org.animotron.statement;

import org.animotron.exception.AnimoException;
import org.animotron.graph.index.Cache;
import org.animotron.inmemory.InMemoryRelationship;
import org.animotron.statement.operator.THE;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;

import static org.animotron.Properties.NAME;
import static org.animotron.Properties.RID;
import static org.animotron.graph.RelationshipTypes.RESULT;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public abstract class AbstractStatement implements Statement {
	
	private String name;

	public AbstractStatement(String name) {
		this.name = name;
	}

//	public AbstractStatement(VALUE reference, VALUE resultRelationshipType) {
//        this(reference);
//	}

	@Override
	public String name() {
		return name;
	}
	
	protected Relationship createResultInMemory(Node node, Relationship r) {
		Relationship res = new InMemoryRelationship(node, r.getEndNode(), RESULT);
		//store to relationship arrow
		RID.set(res, r.getId());
		
		return res;
	}

    @Override
    public Object reference(Relationship r) {
        return null;
    }

    public Node reference(Object reference, boolean ignoreNotFound) throws AnimoException {
        Node node;
        if (reference instanceof Node) {
            node = (Node) reference;
        } else if (reference instanceof Relationship) {
            node = ((Relationship) reference).getEndNode();
        } else {
            return THE._.getOrCreate((String) reference, ignoreNotFound).getEndNode();
        }
        if (NAME.has(node)) {
            return node;
        }
        throw new IllegalArgumentException("Referemce must be the \"THE\" object");
    }

    protected Node createChild(Object reference, boolean ready, boolean ignoreNotFound) throws AnimoException {
        throw new AnimoException(null, "Can't create a child node");
    }


    protected final Node throwCache(Object reference, byte[] hash, boolean ready, boolean ignoreNotFound) throws AnimoException {
        if (ready) {
            Node child = Cache.getNode(hash);
            if (child == null) {
                child = createChild(reference, false, ignoreNotFound);
                Cache.putNode(child, hash);
            }
            return child;
        } else {
            return createChild(reference, false, ignoreNotFound);
        }
    }

    @Override
	public Relationship build(Node parent, Object reference, byte[] hash, boolean ready, boolean ignoreNotFound) throws AnimoException {
		return parent.createRelationshipTo(throwCache(reference, hash, ready, ignoreNotFound), this);
	}

}
