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

import org.animotron.Properties;
import org.animotron.exception.ENotFound;
import org.animotron.graph.AnimoGraph;
import org.animotron.graph.AnimoRelationshipType;
import org.animotron.graph.RelationshipTypes;
import org.animotron.manipulator.OnQuestion;
import org.animotron.manipulator.PFlow;
import org.jetlang.channels.Subscribable;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.RelationshipType;
import org.neo4j.graphdb.Transaction;
import org.neo4j.graphdb.event.ErrorState;
import org.neo4j.graphdb.event.KernelEventHandler;

import static org.animotron.Properties.HASH;
import static org.animotron.graph.AnimoGraph.*;
import static org.neo4j.graphdb.Direction.OUTGOING;

/**
 * Operator 'THE'.
 *
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 */
public class THE extends Operator implements Prepare, KernelEventHandler {

	public static String NAME = "the";

	public static final THE _ = new THE();

	private THE() {
		super(NAME);
	}

	private Node THE_NODE = null;

	public Node THE_NODE() {
		if (THE_NODE == null) {
			synchronized (this) {
				Transaction tx = beginTx();
				try {
					THE_NODE = getOrCreateNode(getROOT(), RelationshipTypes.THE);
					AnimoGraph.getDb().registerKernelEventHandler(this);
					tx.success();
				} finally {
					finishTx(tx);
				}
			}
		}
		return THE_NODE;
	}

	public RelationshipType relationshipType(String name){
		return AnimoRelationshipType.get(name(), name);
	}

	public Relationship get(String name) {
		RelationshipType type = relationshipType(name);
		return THE_NODE().getSingleRelationship(type, OUTGOING);
	}

	public Relationship create(String name, String hash) throws ENotFound {
        //TODO do we really need a reference?
        if (name == null) name = hash;
		Relationship r = create(name);
		HASH.set(r, hash);
		return r;
	}

	private Relationship create(String name) throws ENotFound {
        Relationship r = build(THE_NODE(), name, true);
        Node node = r.getEndNode();
        getTOP().createRelationshipTo(node, RelationshipTypes.TOP);
        return r;
	}

	public Relationship getOrCreate(String name, boolean ignoreNotFound) throws ENotFound {
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
	public Relationship build(Node parent, Object name, boolean ignoreNotFound) {
        Node node = createNode();
        RelationshipType type = relationshipType((String) name);
        Relationship r = parent.createRelationshipTo(node, type);
        Properties.NAME.set(r, name);
        return r;
	}

	@Override
	public Object reference(Relationship r) {
		return Properties.NAME.get(r);
	}

    private Subscribable<PFlow> prepare = new OnQuestion();

	@Override
	public Subscribable<PFlow> onPrepareQuestion() {
        return prepare;
	}

	@Override
	public void beforeShutdown() {
		THE_NODE = null;

	}

	@Override
	public void kernelPanic(ErrorState error) {
		//ignore
	}

	@Override
	public Object getResource() {
		//ignore
		return null;
	}

	@Override
	public ExecutionOrder orderComparedTo(KernelEventHandler other) {
		//ignore
		return null;
	}

}