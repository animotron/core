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
package org.animotron.statement.operator;

import org.animotron.exception.AnimoException;
import org.animotron.exception.ENotFound;
import org.animotron.graph.index.Cache;
import org.animotron.graph.index.State;
import org.animotron.manipulator.OnQuestion;
import org.animotron.manipulator.PFlow;
import org.animotron.statement.AbstractStatement;
import org.jetlang.channels.Subscribable;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;

import static org.animotron.Properties.NAME;
import static org.animotron.graph.AnimoGraph.createNode;
import static org.animotron.graph.AnimoGraph.getROOT;

/**
 * Operator 'THE'.
 *
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 */
public class THE extends AbstractStatement implements Prepare {

	public static final THE _ = new THE();

    public static Node _(String name) {
        try {
            return THE._.getOrCreate(name, true).getEndNode();
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    public static Relationship __(String name) {
        try {
            return THE._.getOrCreate(name, true);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    private THE() { super("the"); }

	public Relationship get(String name) {
        return Cache.getRelationship(name);
	}

	private Relationship create(String name) throws AnimoException {
        Relationship r;
        r = build(getROOT(), name, null, false, true);
        Node node = r.getEndNode();
        Cache.putRelationship(r, name);
        State.TOP.add(node);
        return r;
	}

	public Relationship getOrCreate(String name, boolean ignoreNotFound) throws AnimoException {
		Relationship r = get(name);
		if (r == null) {
            if (ignoreNotFound) {
                r = create(name);
            } else {
                throw new ENotFound(null, "Internal error: \"the:" + name + "\" not found");
            }
		}
		return r;
	}
	
    @Override
    protected Node createChild(Object reference, boolean ready, boolean ignoreNotFound) throws AnimoException {
        Node node = createNode();
        Cache.putNode(node, reference);
        NAME.set(node, reference);
        return node;
    }

	@Override
	public Object reference(Relationship r) {
		return NAME.get(r.getEndNode());
	}

	public Object reference(Node r) {
		return NAME.get(r);
	}

	private Subscribable<PFlow> prepare = new OnQuestion();

	@Override
	public Subscribable<PFlow> onPrepareQuestion() {
        return prepare;
	}

}