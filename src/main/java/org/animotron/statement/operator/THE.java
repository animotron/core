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

import org.animotron.exception.AnimoException;
import org.animotron.exception.ENotFound;
import org.animotron.graph.index.Cache;
import org.animotron.graph.index.State;
import org.animotron.manipulator.OnQuestion;
import org.animotron.manipulator.PFlow;
import org.animotron.statement.AbstractStatement;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;

import static org.animotron.graph.Properties.MODIFIED;
import static org.animotron.graph.Properties.NAME;
import static org.animotron.graph.AnimoGraph.createNode;
import static org.animotron.graph.AnimoGraph.getROOT;
import static org.animotron.graph.Properties.UUID;

/**
 * Operator 'THE'.
 *
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 */
public class THE extends AbstractStatement implements Prepare, Definition {

	public static final THE _ = new THE();

//    public static Node _(String name) {
//        try {
//            return THE._.getOrCreate(name, true).getEndNode();
//        } catch (Exception e) {
//            throw new RuntimeException(e);
//        }
//    }
//
//    public static Relationship __(String name) {
//        try {
//            return THE._.getOrCreate(name, true);
//        } catch (Exception e) {
//            throw new RuntimeException(e);
//        }
//    }

    private THE() { super("the"); }

	public Relationship get(String name) {
        return Cache.RELATIONSHIP.get(name);
	}

	private Relationship create(String name) throws AnimoException {
        Relationship r;
        r = build(getROOT(), name, null, false, true);
        MODIFIED.set(r, System.currentTimeMillis());
        UUID.set(r, java.util.UUID.randomUUID().toString());
        Node node = r.getEndNode();
        Cache.RELATIONSHIP.put(r, name);
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
        Cache.NODE.put(node, reference);
        NAME.set(node, reference);
        return node;
    }

	@Override
	public Object reference(Relationship r) {
		try {
			return NAME.get(r.getEndNode());
		} catch (Exception e) {
			return null;
		}
	}

	public Object reference(Node r) {
		return NAME.get(r);
	}

	@Override
	public OnQuestion onPrepareQuestion() {
        return new OnQuestion() {
			@Override
			public void act(PFlow pf) {
				System.out.println("PREPARE THE");
			}
			
		};
	}
}