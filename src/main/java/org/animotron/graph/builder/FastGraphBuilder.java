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
import org.animotron.statement.operator.DEF;
import org.animotron.utils.MessageDigester;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;

import java.io.IOException;
import java.security.MessageDigest;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import static org.animotron.expression.Expression.__;
import static org.animotron.graph.AnimoGraph.*;
import static org.animotron.graph.Properties.*;
import static org.animotron.utils.MessageDigester.cloneMD;
import static org.animotron.utils.MessageDigester.md;
import static org.animotron.utils.MessageDigester.updateMD;

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

    private List<Object[]> flow = new LinkedList<Object[]>();
    private byte[] hash;

    public FastGraphBuilder () {
        super();
    }

    public FastGraphBuilder (boolean ignoreNotFound) {
        super(ignoreNotFound);
    }

    @Override
    public Relationship endGraph() throws AnimoException {
        Relationship r = null;
        Iterator<Object[]> it = flow.iterator();
        if (it.hasNext()) {
            Object[] o = it.next();
            if (o[1] instanceof DEF) {
                if (o[2] == null) {
                    o[2] = MessageDigester.byteArrayToHex(hash);
                }
                r = build(o, it);
                Node def = r.getEndNode();
                NAME.set(def, o[2]);
                DEFID.set(def, r.getId());
                DEF._.add(r, o[2]);
                modificative(r);
                preparative(r);
            } else {
                r = Cache.RELATIONSHIP.get(hash);
                if (r == null) {
                    o[6] = Cache.NODE.get(hash) != null;
                    r= build(o, it);
                    Cache.RELATIONSHIP.add(r, hash);
                    modificative(r);
                    HASH.set(r, hash);
                    preparative(r);
                }
            }
        }
        return r;
    }

    private Relationship build(Object[] o, Iterator<Object[]> it) throws AnimoException {
        Relationship r = ((Statement) o[1]).build(getROOT(), o[2], hash, true, ignoreNotFound);
        final Node end = r.getEndNode();
        o[3] = r;
        o[4] = end;
        step();
        while (it.hasNext()) {
            build(it.next());
            step();
        }
        return r;
    }

    @Override
    protected Object[] start(Statement statement, Object reference, boolean hasChild) throws AnimoException {
        Object[] parent = hasParent() ? peekParent() : null;
        MessageDigest md = md();
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
    protected byte[] end(Object[] o) {
        MessageDigest md = (MessageDigest) o[0];
        if (!(Boolean) o[7]) {
            updateMD(md, (Statement) o[1]);
        }
        o[8] = hash = md.digest();
        return hash;
    }

    @Override
    public void bind(Relationship r, Object[] p, byte[] hash) throws IOException {
        step();
        Object[] o = {p, r};
        flow.add(o);
    }

    private void build(Object[] item) throws AnimoException {
        Relationship r;
        if (item.length == 2) {
            Node n = (Node) ((Object[]) item[0])[4];
            r = copy(n, (Relationship) item[1]);
            CONTEXT.set(n, true);
        } else {
            Object[] p =  (Object[]) item[5];
            if (p != null) {
                if ((Boolean) p[6]) {
                    item[6] = true;
                    return;
                }
            }
            Statement statement = (Statement) item[1];
            Object reference = item[2];
            byte[] hash = (byte[]) item[8];
            Node parent = (Node) p[4];
            item[6] = Cache.NODE.get(hash) != null;
            r = statement.build(parent, reference, hash, true, ignoreNotFound);
            item[3] = r;
            item[4] = r.getEndNode();
        }
        order(r);
    }

}
