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
import org.animotron.graph.AnimoGraph;
import org.animotron.graph.index.AbstractIndex;
import org.animotron.manipulator.OnQuestion;
import org.animotron.manipulator.PFlow;
import org.animotron.statement.AbstractStatement;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.index.IndexManager;
import org.neo4j.index.bdbje.BerkeleyDbIndexImplementation;

import static org.animotron.graph.AnimoGraph.createNode;
import static org.animotron.graph.AnimoGraph.getROOT;
import static org.animotron.graph.Properties.*;
import static org.neo4j.graphdb.Direction.INCOMING;

/**
 * Operator 'DEF'.
 *
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 */
public class DEF extends AbstractStatement implements Prepare, Definition {

	public static final DEF _ = new DEF();

//    public static Node _(String name) {
//        try {
//            return DEF._.getOrCreate(name, true).getEndNode();
//        } catch (Throwable t) {
//            throw new RuntimeException(e);
//        }
//    }
//
//    public static Relationship __(String name) {
//        try {
//            return DEF._.getOrCreate(name, true);
//        } catch (Throwable t) {
//            throw new RuntimeException(e);
//        }
//    }
    
    private DEF() { super("def"); }
    
    private AbstractIndex<Relationship> def = new AbstractIndex<Relationship>(name()) {
        @Override
        public void init(IndexManager index) {
            init(index.forRelationships(name, BerkeleyDbIndexImplementation.DEFAULT_CONFIG));
        }
    };

    public void init(IndexManager index) {
        def.init(index);
	}

	public void add(Relationship r, Object name) {
        def.add(r, name);
	}

	public Node getDef(Node rev) {
        if (DEFID.has(rev)) {
        	return rev;
        }
        return null;
	}

	public Relationship get(Object name) {
        return def.get(name);
	}
	
	public Node getDefNode(Node rev) {
        Relationship ar = rev.getSingleRelationship(ASHIFT._, INCOMING);
        return ar == null ? null : ar.getStartNode();
	}

//	public Relationship get(Node rev) {
//        Relationship ar = rev.getSingleRelationship(ASHIFT._, Direction.INCOMING);
//        return ar == null ? null : ar.getStartNode().getSingleRelationship(DEF._, Direction.INCOMING);
//	}

	public static Relationship getRelationship(Node node) {
		try {
			return AnimoGraph.getDb().getRelationshipById((Long) DEFID.get(node));
		} catch (Throwable e) {
		}
        return  node.getSingleRelationship(DEF._, INCOMING);
	}

	public static Relationship getDef(Relationship arev) {
        return getDefR(arev.getStartNode());
	}

	public static Relationship getDefR(Node defNode) {
        return defNode.getSingleRelationship(DEF._, INCOMING);
	}

	private Relationship create(String name) throws AnimoException {
        Relationship r = build(getROOT(), name, null, false, true);
        Node n = r.getEndNode();
        NAME.set(n, name);
        add(r, name);
        return r;
	}

	public Relationship getOrCreate(String name, boolean ignoreNotFound) throws AnimoException {
		Relationship r = get(name);
		if (r == null) {
            if (ignoreNotFound) {
                r = create(name);
            } else {
                throw new ENotFound(null, "Internal error: \"def:" + name + "\" not found");
            }
		}
		return r;
	}
	
    @Override
    protected Node createChild(Object name, boolean ready, boolean ignoreNotFound) throws AnimoException {
        return createNode();
    }

    @Override
    public Relationship build(Node parent, Object reference, byte[] hash, boolean ready, boolean ignoreNotFound) throws AnimoException {
        CONTEXT.set(parent, true);
        Node n = createChild(reference, ready, ignoreNotFound);
        Relationship r = parent.createRelationshipTo(n, this);
        DEFID.set(n, r.getId());
        return r;
    }


    @Override
	public Object reference(Relationship r) {
		try {
			return NAME.get(r.getEndNode());
		} catch (Throwable t) {
			return null;
		}
	}

	public Object reference(Node n) {
		return NAME.get(n);
	}
	
//    @Override
//	public OnQuestion onCalcQuestion() {
//		return new Calc();
//    }
//
//    class Calc extends OnQuestion {
//	
//		@Override
//		public void act(final PFlow pf) throws Throwable {
//			pf.sendAnswer(ASHIFT._.actualRelationship(pf.getOP()));
//		}
//    }


	@Override
	public OnQuestion onPrepareQuestion() {
        return new OnQuestion() {
			@Override
			public void act(PFlow pf) {
				System.out.println("PREPARE DEF");
			}
			
		};
	}
}