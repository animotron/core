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
import org.animotron.graph.index.AbstractIndex;
import org.animotron.manipulator.OnQuestion;
import org.animotron.manipulator.PFlow;
import org.animotron.statement.AbstractStatement;
import org.neo4j.graphdb.Direction;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.index.IndexManager;
import org.neo4j.index.bdbje.BerkeleyDbIndexImplementation;

import static org.animotron.graph.AnimoGraph.*;
import static org.animotron.graph.Properties.*;
import static org.animotron.graph.RelationshipTypes.AREV;
import static org.animotron.utils.MessageDigester.uuid;
import static org.neo4j.graphdb.Direction.OUTGOING;

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
//        } catch (Throwable t) {
//            throw new RuntimeException(e);
//        }
//    }
//
//    public static Relationship __(String name) {
//        try {
//            return THE._.getOrCreate(name, true);
//        } catch (Throwable t) {
//            throw new RuntimeException(e);
//        }
//    }
    
    private static final String name = "the";

    private THE() { super(name); }
    
    private AbstractIndex<Relationship> the = new AbstractIndex<Relationship>(name) {
        @Override
        public void init(IndexManager index) {
            init(index.forRelationships(name, BerkeleyDbIndexImplementation.DEFAULT_CONFIG));
        }
    };
    
    public void setActualRevision(Node node, Node rev) {
        node.getSingleRelationship(AREV, OUTGOING).delete();
        node.createRelationshipTo(rev, AREV);
    }

    public Node getActualRevision(Node node) {
        return node.getSingleRelationship(AREV, OUTGOING).getEndNode();
    }

    public Node getActualRevision(Relationship relationship) {
        return getActualRevision(relationship.getEndNode());
    }

    public Node getActualEndNode(Relationship r) {
    	Node n = r.getEndNode();

		if (r.isType(REF._) || r.isType(THE._))
			return getActualRevision(n);
		
		return n;
    }

    public void init(IndexManager index) {
        the.init(index);
	}

	public void add(Relationship r, Object name) {
        the.add(r, name);
	}

	public Node getThe(Node rev) {
        return getDb().getNodeById((Long) THEID.get(rev));
	}

	public Relationship get(Object name) {
        return the.get(name);
	}

	public Relationship get(Node node) {
        return getActualRevision(node).getSingleRelationship(THE._, Direction.INCOMING);
	}

	private Relationship create(String name) throws AnimoException {
        Relationship r = build(getROOT(), name, null, false, true);
        Node n = r.getEndNode();
        n.createRelationshipTo(n, AREV);
        UUID.set(r, uuid().toString());
        add(r, name);
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
    protected Node createChild(Object name, boolean ready, boolean ignoreNotFound) throws AnimoException {
        Node node = createNode();
        NAME.set(node, name);
        return node;
    }

	@Override
	public Object reference(Relationship r) {
		try {
			return NAME.get(r.getEndNode());
		} catch (Throwable t) {
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