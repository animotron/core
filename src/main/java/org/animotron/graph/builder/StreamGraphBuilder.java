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
import org.animotron.statement.Statement;
import org.animotron.statement.operator.THE;
import org.animotron.statement.relation.Relation;
import org.animotron.utils.MessageDigester;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;

import java.security.MessageDigest;
import java.util.Stack;

import static org.animotron.Properties.HASH;
import static org.animotron.Properties.NAME;
import static org.animotron.graph.AnimoGraph.*;

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
 * start(String prefix, String ns, String reference, String value)
 * start(Statement statement, String prefix, String ns, String reference, String value)
 * end()
 * 
 * getRelationship()
 * 
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 */
public class StreamGraphBuilder extends GraphBuilder {
	
	private Node root, parent;
	private Stack<Object[]> stack;
    private int order;
    private String hash;

    @Override
	public void startGraph() {
        stack = new Stack<Object[]>();
        parent = root = createNode();
        order = 0;
        the = null;
	}

    @Override
	public void endGraph() throws AnimoException {
        if (the == null) {
            Relationship old = THE._.get(hash);
            if (old == null) {
                the = THE._.THE_NODE().createRelationshipTo(root, THE._.relationshipType(hash));
                NAME.set(the, hash);
                HASH.set(the, hash);
                catcher.creative(the);
            } else {
                if (hash.equals(HASH.get(old))) {
                    catcher.destructive(root);
                    the = old;
                } else {
                    the = THE._.THE_NODE().createRelationshipTo(root, THE._.relationshipType(hash));
                    NAME.set(the, hash);
                    HASH.set(the, hash);
                    catcher.renew(old, the);
                }
            }
        } else {
            root.delete();
        }
	}

	@Override
    public void start(Statement statement, String reference) throws AnimoException {
//        if (flow.isEmpty() && !(statement instanceof THE)) {
//            start(THE._, null);
//        }
        if (order % 100 == 0)
            System.out.println(order);
        Relationship r = statement.build(parent, reference, order++, ignoreNotFound);
        if (!(statement instanceof Relation)) {
            parent = r.getEndNode();
        }
        MessageDigest md = statement.hash(reference);
		Object[] item = {
				statement,	    // 0  statement
				md,             // 1  message digest
				r,              // 2  node
                reference       // 3 reference
			};
		stack.push(item);
	}
	
    @Override
	public void end() {
		Object[] item = stack.pop();
        Statement statement = (Statement) item[0];
        byte[] md = ((MessageDigest) item[1]).digest();
        hash = MessageDigester.byteArrayToHex(md);
        if (!(statement instanceof Relation)) {
            Relationship r = (Relationship) item[2];
            if (statement instanceof THE) {
                String  reference = (String) item[3];
                Relationship old = THE._.get(reference);
                if (old == null) {
                    the = THE._.THE_NODE().createRelationshipTo(r.getEndNode(), r.getType());
                    NAME.set(the, hash);
                    HASH.set(the, hash);
                    catcher.creative(the);
                    r.delete();
                } else {
                    if (hash.equals(HASH.get(old))) {
                        catcher.destructive(r);
                        the = old;
                    } else {
                        the = THE._.THE_NODE().createRelationshipTo(r.getEndNode(), r.getType());
                        NAME.set(the, hash);
                        HASH.set(the, hash);
                        catcher.renew(old, the);
                        r.delete();
                    }
                }
            } else {
                Node node = getCache(hash);
                if (node == null){
                    createCache(r.getEndNode(), hash);
                    catcher.creative(r);
                    HASH.set(r, hash);
                } else {
                    catcher.destructive(r);
                }
            }
        }
        if (!stack.empty()) {
            ((MessageDigest) stack.peek()[1]).update(md);
        }
	}

}
