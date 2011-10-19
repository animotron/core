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

import java.io.IOException;
import java.security.MessageDigest;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

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

	private List<Object[]> flow;
    private Relationship relationship;
    private Node root;
    private byte[] hash;

    @Override
    protected void fail(Exception e) {
        if (root != null) {
            destructive(root);
        }
    }

    @Override public Relationship relationship() {
        return relationship;
    }

    @Override
    public void startGraph() {
		flow = new LinkedList<Object[]>();
        root = null;
	}
	
	@Override
    public void endGraph() throws AnimoException, IOException {
        Iterator<Object[]> it = flow.iterator();
        if (it.hasNext()) {
            Object[] o = it.next();
            Statement statement = (Statement) o[1];
            relationship = Cache.getRelationship(hash);
            if (relationship == null) {
                Object reference = statement instanceof THE && o[2] == null ? MessageDigester.byteArrayToHex(hash) : o[2];
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
                            destructive(i);
                        }
                        int order = 1;
                        for (Relationship i : end.getRelationships(OUTGOING)) {
                            order(copy(start, i), order++);
                            i.delete();
                        }
                    }
                    creative(relationship);
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
    protected Object[] start(Statement statement, Object reference, boolean hasChild) throws AnimoException {
		Object[] parent = hasParent() ? peekParent() : null;
        MessageDigest md = MessageDigester.md();
        byte[] hash = null;
        boolean ready = !hasChild && reference != null;
        if (ready) {
            updateMD(md, reference);
            hash = cloneMD(md).digest();
            updateMD(md, statement);
        } else if (reference != null) {
            updateMD(md, reference);
        }
		Object[] o = {
                md,             // 0 message digest    
				statement,	    // 1 statement
                reference,      // 2 name or value
				null,	 	    // 3 current relationship
				null,		    // 4 current node
				parent, 	    // 5 parent item
                false,          // 6 is done?
                ready,          // 7 is ready?
                hash            // 8 hash
			};
		flow.add(o);
        return o;
	}
	
	@Override
    protected byte[] end(Object[] o, boolean hasChild) {
        MessageDigest md = (MessageDigest) o[0];
        if (!(Boolean) o[7]) {
            updateMD(md, (Statement) o[1]);
        }
        o[8] = hash = md.digest();
        return hash;
	}

	private void build(Object[] item) throws AnimoException {
		Object[] p =  (Object[]) item[5];
		if (p != null) {
			if ((Boolean) p[6]) {
				item[6] = true;
				return;
			}
		}
        Relationship r;
        Statement statement = (Statement) item[1];
        Object reference = item[2];
        byte[] hash = (byte[]) item[8];
        Node parent = (Node) p[4];
        item[6] = Cache.getNode(hash) != null;
        r = statement.build(parent, reference, hash, true, ignoreNotFound);
        item[3] = r;
        item[4] = r.getEndNode();
        order(r);
	}
	
}
