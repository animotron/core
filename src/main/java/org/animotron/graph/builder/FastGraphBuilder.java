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
import org.animotron.graph.Cache;
import org.animotron.graph.RelationshipTypes;
import org.animotron.statement.Statement;
import org.animotron.statement.operator.THE;
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
import static org.animotron.graph.Cache.key;
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
 * relationship()
 * 
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 */
public class FastGraphBuilder extends GraphBuilder {

	private Stack<Object[]> statements;
	private List<Object[]> flow;
    private Relationship relationship;
    private Node root;

    @Override
    protected void fail(Exception e) {
        if (root != null) {
            catcher.destructive(root);
        }
    }

    @Override public Relationship relationship() {
        return relationship;
    }

    @Override
    public void startGraph() {
		statements = new Stack<Object[]>();
		flow = new LinkedList<Object[]>();
        root = null;
	}
	
	@Override
    public void endGraph() throws AnimoException {
        Iterator<Object[]> it = flow.iterator();
        if (it.hasNext()) {
            Object[] o = it.next();
            Statement statement = (Statement) o[0];
            byte[] hash = (byte[]) o[7];
            relationship = Cache.getRelationship(hash);
            if (relationship == null) {
                Object reference = statement instanceof THE && o[1] == null ? MessageDigester.byteArrayToHex(hash) : o[1];
                root = createNode();
                Relationship r = statement.build(root, reference, hash, true, ignoreNotFound);
                Node end = r.getEndNode();
                o[3] = r;
                o[4] = end;
                step();
                while (it.hasNext()) {
                    build(it.next());
                    step();
                }
                if (statement instanceof THE) {
                    relationship = Cache.getRelationship(reference);
                    if (relationship == null) {
                        relationship = getSTART().createRelationshipTo(end, r.getType());
                        Cache.putRelationship(relationship, reference);
                        getTOP().createRelationshipTo(end, RelationshipTypes.TOP);
                    } else {
                        Node start = relationship.getEndNode();
                        for (Relationship i : start.getRelationships(OUTGOING)) {
                            catcher.destructive(i);
                        }
                        int order = 1;
                        for (Relationship i : end.getRelationships(OUTGOING)) {
                            order(copy(start, i), order++);
                            i.delete();
                        }
                    }
                    catcher.creative(relationship);
                } else {
                    relationship = getSTART().createRelationshipTo(end, r.getType());
                }
                Cache.putRelationship(relationship, hash);
                HASH.set(relationship, key(hash));
                r.delete();
                root.delete();
            }
        }
	}

    @Override
	public void start(Statement statement, Object reference) {
		Object[] parent = statements.empty() ? null : statements.peek();
        MessageDigest md = MessageDigester.md();
		Object[] item = {
				statement,	    // 0 statement
                reference,      // 1 name or value
				md, 		    // 2 message digest
				null,	 	    // 3 current relationship
				null,		    // 4 current node
				parent, 	    // 5 parent item
				false,          // 6 is done before?
                null,           // 7 hash
                false           // 8 ready
			};
		statements.push(item);
		flow.add(item);
	}
	
	@Override
    public void end() {
		Object[] current = statements.pop();
        Statement statement = (Statement) current[0];
        Object reference = current[1];
        MessageDigest md = (MessageDigest) current[2];
        updateMD(md, reference);
        byte[] hash = cloneMD(md).digest();
        updateMD(md, statement);
		Object[] parent = (Object[]) current[5];
		if (parent != null) {
			updateMD((MessageDigest) parent[2], statement, reference);
		}
		current[7] = hash;
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
        Relationship r;
        Statement statement = (Statement) item[0];
        Object reference = item[1];
        byte[] hash = (byte[]) item[7];
        Node parent = (Node) p[4];
        item[6] = Cache.getNode(hash) != null;
        r = statement.build(parent, reference, hash, true, ignoreNotFound);
        item[3] = r;
        item[4] = r.getEndNode();
        order(r);
	}
	
}
