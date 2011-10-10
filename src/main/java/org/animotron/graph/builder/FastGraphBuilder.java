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
import org.animotron.statement.operator.AN;
import org.animotron.statement.operator.THE;
import org.animotron.statement.relation.Relation;
import org.animotron.statement.value.Value;
import org.animotron.utils.MessageDigester;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;

import java.security.MessageDigest;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Stack;

import static org.animotron.Properties.HASH;
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
 * start(VALUE prefix, VALUE ns, VALUE reference, VALUE value)
 * start(Statement statement, VALUE prefix, VALUE ns, VALUE reference, VALUE value)
 * end()
 * 
 * getRelationship()
 * 
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 */
public class FastGraphBuilder extends GraphBuilder {

	private Stack<Object[]> statements;
	private List<Object[]> flow;

	@Override
    public void startGraph() {
		statements = new Stack<Object[]>();
		flow = new LinkedList<Object[]>();
	}
	
	@Override
    public void endGraph() throws AnimoException {
        if (!statements.empty()) {
            end();
        }
        Iterator<Object[]> it = flow.iterator();
        Object[] item = it.next();
        String hash = hash(item);
        String name = item[1] != null ? (String) item[1] : hash;
        the = THE._.get(name);
        if (the != null) {
            if (HASH.has(the)) {
                String h = (String) HASH.get(the);
                if (h == null) {
                    catcher.creative(the);
                    HASH.set(the, hash);
                } else if (!h.equals(hash)) {
                    catcher.renew(the);
                    HASH.set(the, hash);
                } else {
                    return;
                }
            } else {
                catcher.creative(the);
                HASH.set(the, hash);
            }
        } else {
            the = THE._.create(name, hash);
            catcher.creative(the);
        }
        item[3] = the;
        item[4] = the.getEndNode();
        order = 0;
        while(it.hasNext()) {
            step();
            build(it.next());
        }
	}

    @Override
	public void start(Statement statement, Object reference) {
        if (flow.isEmpty() && !(statement instanceof THE)) {
            start(THE._, null);
        }
		Object[] parent = null;
		if (!statements.empty()) {
			if (statement instanceof THE) {
				start(AN._, reference);
				end();
			} else {
				parent = statements.peek();
			}
		}
        boolean ready = statement instanceof Value || ignoreNotFound;
        MessageDigest md = statement.hash(reference);
		Object[] item = {
				statement,	    // 0 statement
                reference,      // 1 name or value
				md, 		    // 2 message digest
				null,	 	    // 3 current relationship
				null,		    // 4 current node
				parent, 	    // 5 parent item
				false,		    // 6 is done?
                ready           // 7 is ready for cache?
			};
		statements.push(item);
		flow.add(item);
	}
	
	@Override
    public void end() {
		Object[] current = statements.pop();
		byte[] hash = ((MessageDigest) current[2]).digest();
		Object[] parent = (Object[]) current[5];
		if (parent != null) {
			((MessageDigest) (parent[2])).update(hash);
            if (parent[0] instanceof Value) {
                parent[7] = false;
            }
		}
		current[2] = hash;
	}

    @Override
    public void fail(Exception e) {
        catcher.destructive((Relationship)flow.get(0)[3]);
    }

    //TODO: Store hash for every node as byte[]
	//TODO: Build graph via single thread in sync and async modes 
	
	private void build(Object[] item) throws AnimoException {
		Object[] p =  (Object[]) item[5];
		if (p != null) {
			if ((Boolean) p[6]) {
				item[6] = true; 
				return;
			}
		}
        Node parent = (Node) p[4];
        if (parent == null)
            //"Internal error: parent can not be null."
            throw new AnimoException((Relationship)item[3]);
        Relationship r;
        Statement statement = (Statement) item[0];
        Object reference = item[1];
        boolean ready = (Boolean) item[7];
        if (!(statement instanceof Relation || statement instanceof Value)) {
            byte[] hash = (byte[]) item[2];
            Node node = getCache(hash);
            if (node == null) {
                r = statement.build(parent, reference, ready);
                createCache(r.getEndNode(), hash);
            } else {
                r = parent.createRelationshipTo(node, statement.relationshipType());
                item[6] = true;
            }
        } else {
            r = statement.build(parent, reference, ready);
        }
        if (r != null) {
            item[3] = r;
            item[4] = r.getEndNode();
            order(r, order);
        }
	}
	
	private String hash(Object[] item) {
        return MessageDigester.byteArrayToHex((byte[]) item[2]);
	}
	
}
