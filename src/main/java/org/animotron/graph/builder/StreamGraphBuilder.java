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
package org.animotron.graph.builder;

import org.animotron.exception.AnimoException;
import org.animotron.graph.index.Cache;
import org.animotron.graph.index.Order;
import org.animotron.statement.Statement;
import org.animotron.statement.operator.THE;
import org.animotron.utils.MessageDigester;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.index.IndexHits;

import java.io.IOException;
import java.security.MessageDigest;

import static org.animotron.graph.AnimoGraph.*;
import static org.animotron.graph.Properties.*;
import static org.animotron.graph.RelationshipTypes.AREV;
import static org.animotron.graph.RelationshipTypes.REV;
import static org.animotron.statement.operator.Utils.freeze;
import static org.animotron.statement.operator.Utils.unfreeze;
import static org.animotron.utils.MessageDigester.*;

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
public class StreamGraphBuilder extends GraphBuilder {

    private Node root;
    private Relationship relationship, r;
    private byte[] hash;

    @Override
    public void startGraph() {
        root = createNode();
	}

    @Override
    public Relationship relationship() {
        return relationship;
    }

    @Override
    public void fail(Throwable t) {
        destructive(root);
    }

    @Override
    public void endGraph() throws AnimoException {
        relationship = Cache.RELATIONSHIP.get(hash);
        if (r.isType(THE._)) {
            Node end = r.getEndNode();
            Object reference;
            if (!NAME.has(end)) {
                reference = byteArrayToHex(hash);
                NAME.set(end, reference);
            } else {
                reference = NAME.get(end);
            }
            if (relationship == null) {
                relationship = THE._.get(reference);
                if (relationship == null) {
                    relationship = getROOT().createRelationshipTo(end, THE._);
                    UUID.set(relationship, uuid().toString());
                    HASH.set(relationship, hash);
                    THEID.set(end, end.getId());
                    THE._.add(relationship, reference);
                    Cache.RELATIONSHIP.add(relationship, hash);
                    end.createRelationshipTo(end, AREV);
                } else {
                    Node n = relationship.getEndNode();
                    Node rn = THE._.getActualRevision(n);
                    freeze(rn);
                    Relationship rr = rn.createRelationshipTo(end, REV);
                    UUID.set(rr, uuid().toString());
                    HASH.set(rr, hash);
                    THEID.set(end, n.getId());
                    Cache.RELATIONSHIP.add(rr, hash);
                    THE._.setActualRevision(n, end);
                }
                r.delete();
            } else {
                unfreeze(end);
                Node nn = createNode();
                copyProperties(end, nn);
                int i = 1;
                IndexHits<Relationship> hits = Order._.queryDown(end);
                try {
                    for (Relationship r : hits) {
                        order(copy(nn, r), i++);
                    }
                } finally {
                    hits.close();
                }
                relationship = THE._.get(reference);
                Node n = relationship.getEndNode();
                Node rn = THE._.getActualRevision(n);
                freeze(rn);
                Relationship rr = rn.createRelationshipTo(nn, REV);
                UUID.set(rr, uuid().toString());
                HASH.set(rr, hash);
                THE._.setActualRevision(n, nn);
                r.delete();
            }
        } else {
            if (relationship == null) {
                relationship = copy(getROOT(), r);
                HASH.set(relationship, hash);
                Cache.RELATIONSHIP.add(relationship, hash);
                r.delete();
                destructive(root);
            } else {
                r.delete();
                destructive(root);
                return;
            }
        }
        preparative(relationship);
        modified(relationship);
	}

	@Override
    protected Object[] start(Statement statement, Object reference, boolean hasChild) throws AnimoException, IOException {
        MessageDigest md = MessageDigester.md();
        Node parent = hasParent() ? ((Relationship) peekParent()[1]).getEndNode() :  root;
        boolean ready = !hasChild && reference != null;
        byte[] hash = null;
        if (ready) {
            updateMD(md, reference);
            hash = cloneMD(md).digest();
            updateMD(md, statement);
        } else if (reference != null) {
            updateMD(md, reference);
        }
        Relationship r = statement.build(parent, reference, hash, ready, ignoreNotFound);
        Object[] o = {
                md,                 // 0 message digest
                r,                  // 1 node
                ready,              // 2 is hasChild?
                order(),            // 3 order
                statement,          // 4 statement
                reference           // 5 reference
            };
        step();
        return o;
	}

    @Override
	protected byte[] end(Object[] item, boolean hasChild) throws AnimoException {
        r = (Relationship) item[1];
        MessageDigest md = (MessageDigest) item[0];
        if (!(Boolean) item[2]) {
            updateMD(md, (Statement) item[4]);
            hash = md.digest();
            Node node = Cache.NODE.get(hash);
            if (node == null) {
                Cache.NODE.add(r.getEndNode(), hash);
            } else {
                Relationship old = r;
                r = copy(old, node);
                destructive(old);
            }
        } else {
            hash = md.digest();
        }
        order(r, (Integer) item[3]);
        return hash;
	}

    @Override
    public void bind(Relationship r, Object[] o, byte[] hash) throws IOException {
        step();
        Node n = ((Relationship) o[1]).getEndNode();
        order(copy(n , r));
        CONTEXT.set(n, true);
    }

}
