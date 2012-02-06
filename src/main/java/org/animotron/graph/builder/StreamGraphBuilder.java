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
import org.animotron.statement.Statement;
import org.animotron.statement.operator.THE;
import org.animotron.utils.MessageDigester;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;

import java.io.IOException;
import java.security.MessageDigest;

import static org.animotron.graph.AnimoGraph.*;
import static org.animotron.graph.Properties.*;
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
    public void fail(Throwable e) {
        destructive(root);
    }

    @Override
    public void endGraph() throws AnimoException {
        relationship = Cache.getRelationship(hash);
        if (relationship == null) {
            long timestamp = System.currentTimeMillis();
            relationship = copy(getROOT(), r);
            Cache.putRelationship(relationship, hash);
            HASH.set(relationship, hash);
            if (relationship.isType(THE._)) {
                Node node = relationship.getEndNode();
                if (!NAME.has(node)) {
                    NAME.set(node, byteArrayToHex(hash));
                }
            }
            CREATED.set(relationship, timestamp);
            MODIFIED.set(relationship, timestamp);
            dropCache(relationship);
        }
        destructive(root);
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
            Node node = Cache.getNode(hash);
            if (node == null) {
                Cache.putNode(r.getEndNode(), hash);
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
