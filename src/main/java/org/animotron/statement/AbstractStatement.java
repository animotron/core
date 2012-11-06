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
package org.animotron.statement;

import org.animotron.exception.AnimoException;
import org.animotron.graph.index.Cache;
import org.animotron.inmemory.InMemoryRelationship;
import org.animotron.statement.operator.DEF;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;

import static org.animotron.graph.Properties.*;
import static org.animotron.graph.RelationshipTypes.RESULT;
import static org.animotron.statement.operator.Utils.unfreeze;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public abstract class AbstractStatement implements Statement {
	
	private String[] name;

	public AbstractStatement(String... name) {
		this.name = name;
	}

//	public AbstractStatement(VALUE reference, VALUE resultRelationshipType) {
//        this(reference);
//	}

    @Override
    public String name() {
        return name[0];
    }
    @Override
    public String[] names() {
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
            return DEF._.getOrCreate((String) reference, ignoreNotFound).getEndNode();
        }
        if (NAME.has(node)) {
            return node;
        }
        throw new IllegalArgumentException("Referemce must be the \"DEF\" object");
    }

    protected Node createChild(Object reference, boolean ready, boolean ignoreNotFound) throws AnimoException {
        throw new AnimoException(null, "Can't create a child node");
    }


    protected final Node throwCache(Object reference, byte[] hash, boolean ready, boolean ignoreNotFound) throws AnimoException {
        if (ready) {
            Node child = Cache.NODE.get(hash);
            if (child == null) {
                child = createChild(reference, false, ignoreNotFound);
                Cache.NODE.add(child, hash);
            } else {
                unfreeze(child);
            }
            return child;
        } else {
            return createChild(reference, false, ignoreNotFound);
        }
    }

    @Override
	public Relationship build(Node parent, Object reference, byte[] hash, boolean ready, boolean ignoreNotFound) throws AnimoException {
        CONTEXT.set(parent, true);
		return parent.createRelationshipTo(throwCache(reference, hash, ready, ignoreNotFound), this);
	}

}
