/*
 *  Copyright (C) 2011 The Animo Project
 *  http://animotron.org
 *
 *  This program is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public License
 *  as published by the Free Software Foundation; either version 2
 *  of the License, or (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02129, USA.
 */
package org.animotron.graph.builder;

import org.animotron.exception.AnimoException;
import org.animotron.graph.RelationshipTypes;
import org.animotron.statement.Statement;
import org.animotron.statement.ml.ELEMENT;
import org.animotron.statement.operator.THE;
import org.animotron.utils.MessageDigester;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;

import java.security.MessageDigest;
import java.util.Stack;

import static org.animotron.Properties.HASH;
import static org.animotron.Properties.NAME;
import static org.animotron.graph.AnimoGraph.*;
import static org.neo4j.graphdb.Direction.INCOMING;
import static org.neo4j.graphdb.Direction.OUTGOING;

/**
 * Animo graph builder, it do optimization/compression and 
 * inform listeners on store/delete events.
 * 
 * Direct graph as input from top element to bottom processing strategy.
 * 
 * Methods to use:
 * 
 * startGraph()
 * endGraph()
 * 
 * start(VALUE prefix, VALUE ns, VALUE reference, VALUE value)
 * start(Statement statement, VALUE prefix, VALUE ns, VALUE reference, VALUE value)
 * end()
 * 
 * getRelationship()
 * 
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 */
public class MLGraphBuilder extends GraphBuilder {
	
	private Node root, parent;
	private Stack<Object[]> stack;
    private byte[] md;

    @Override
	public void startGraph() {
        stack = new Stack<Object[]>();
        root = createNode();
	}

    @Override
	public void endGraph() throws AnimoException {
        MessageDigest m = THE._.hash(null);
        m.update(md);
        String hash = MessageDigester.byteArrayToHex(m.digest());
        Relationship old = THE._.get(hash);
        if (old == null) {
            the = getSTART().createRelationshipTo(root, THE._.relationshipType(hash));
            NAME.set(root, hash);
            NAME.set(the, hash);
            HASH.set(the, hash);
            catcher.creative(the);
        } else {
            if (hash.equals(HASH.get(old))) {
                destructive(root);
                the = old;
            } else {
                the = getSTART().createRelationshipTo(root, THE._.relationshipType(hash));
                NAME.set(root, hash);
                NAME.set(the, hash);
                HASH.set(the, hash);
                catcher.renew(old, the);
            }
        }
        getTOP().createRelationshipTo(the.getEndNode(), RelationshipTypes.TOP);
	}

	@Override
    public void start(Statement statement, Object reference) throws AnimoException {
        step();
        MessageDigest hash = statement.hash(reference);
        parent = stack.empty() ? root : ((Relationship) stack.peek()[1]).getEndNode();
        boolean ready = !(statement instanceof ELEMENT) && reference != null;
        Relationship r = statement.build(parent, reference, ready);
        Object[] item = {
                hash,           // 0  message digest
                r,              // 1  node
                ready,          // 2 is ready for cache?
                order           // 3 order;
            };
		stack.push(item);
	}
	
    @Override
	public void end() throws AnimoException {
		Object[] item = stack.pop();
        md = ((MessageDigest) item[0]).digest();
        Relationship r = (Relationship) item[1];
        int order = (Integer) item[3];
        if (!((Boolean)item[2])) {
            Node node = getCache(md);
            if (node == null) {
                createCache(r.getEndNode(), md);
                order(r, order);
            } else {
                order(r.getStartNode().createRelationshipTo(node, r.getType()), order);
                destructive(r);
            }
        } else {
            order(r, order);
        }
        if (!stack.empty()) {
            ((MessageDigest) stack.peek()[0]).update(md);
        }
	}

    @Override
    public void fail(Exception e) {
        catcher.destructive(root);
    }

    private void destructive(Relationship r) {
        Node node = r.getEndNode();
        r.delete();
        if (!node.hasRelationship(INCOMING)) {
            destructive(node);
        }
    }

    private void destructive(Node n) {
        for (Relationship r : n.getRelationships(OUTGOING)) {
            destructive(r);
        }
        n.delete();
    }

}
