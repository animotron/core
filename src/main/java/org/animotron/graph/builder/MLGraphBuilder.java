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
import org.animotron.statement.operator.THE;
import org.animotron.statement.value.Value;
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
 * start(STRING prefix, STRING ns, STRING reference, STRING value)
 * start(Statement statement, STRING prefix, STRING ns, STRING reference, STRING value)
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
            the = THE._.THE_NODE().createRelationshipTo(root, THE._.relationshipType(hash));
            NAME.set(root, hash);
            NAME.set(the, hash);
            HASH.set(the, hash);
            catcher.creative(the);
        } else {
            if (hash.equals(HASH.get(old))) {
                destructive(root);
                the = old;
            } else {
                the = THE._.THE_NODE().createRelationshipTo(root, THE._.relationshipType(hash));
                NAME.set(root, hash);
                NAME.set(the, hash);
                HASH.set(the, hash);
                catcher.renew(old, the);
            }
        }
        getTOP().createRelationshipTo(the.getEndNode(), RelationshipTypes.TOP);
	}

	@Override
    public void start(Statement statement, String reference) throws AnimoException {
        step();
        MessageDigest hash = statement.hash(reference);
        parent = stack.empty() ? root : ((Relationship) stack.peek()[2]).getEndNode();
        Relationship r = statement.build(parent, reference, ignoreNotFound);
        Object[] item = {
                statement,	    // 0  statement
                hash,           // 1  message digest
                r               // 2  node
            };
		stack.push(item);
	}
	
    @Override
	public void end() throws AnimoException {
		Object[] item = stack.pop();
        Statement statement = (Statement) item[0];
        md = ((MessageDigest) item[1]).digest();
        if (!(statement instanceof Value)) {
            Node node = getCache(md);
            Relationship r = (Relationship) item[2];
            if (node == null) {
                createCache(r.getEndNode(), md);
            } else {
                r = r.getStartNode().createRelationshipTo(node, r.getType());
                destructive(r);
            }
            order(r, order);
        }
        if (!stack.empty()) {
            ((MessageDigest) stack.peek()[1]).update(md);
        }
	}

    @Override
    public void fail(Exception e) {
        catcher.destructive(root);
    }

    private void destructive(Relationship r) {
        Node node = r.getEndNode();
        r.delete();
        if (node.hasRelationship(INCOMING)) {
            node.delete();
        }
    }

    private void destructive(Node n) {
        for (Relationship r : n.getRelationships(OUTGOING)) {
            destructive(r);
        }
        n.delete();
    }

}
