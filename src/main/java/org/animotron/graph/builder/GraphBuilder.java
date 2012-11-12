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
import org.animotron.expression.AbstractExpression;
import org.animotron.graph.Properties;
import org.animotron.graph.index.Cache;
import org.animotron.graph.index.Order;
import org.animotron.graph.serializer.DigestSerializer;
import org.animotron.manipulator.Manipulators;
import org.animotron.statement.Statement;
import org.animotron.statement.animo.update.CHANGE;
import org.animotron.statement.link.LINK;
import org.animotron.statement.operator.DEF;
import org.animotron.statement.operator.REF;
import org.animotron.statement.value.VALUE;
import org.animotron.utils.MessageDigester;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.Transaction;
import org.neo4j.kernel.DeadlockDetectedException;

import java.io.IOException;
import java.security.MessageDigest;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Stack;

import static org.animotron.graph.AnimoGraph.*;
import static org.animotron.graph.Properties.HASH;
import static org.animotron.graph.Properties.NAME;
import static org.animotron.graph.Properties.DEFID;
import static org.animotron.graph.Properties.CONTEXT;
import static org.animotron.utils.MessageDigester.*;
import static org.neo4j.graphdb.Direction.OUTGOING;

/**
 * Animo graph builder, it do optimization/compression and
 * inform listeners on store/delete events.
 *
 * Direct graph as input from top element to bottom processing strategy.
 *
 * Methods to use:
 *
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
public class GraphBuilder {

    Transaction tx;
    private int order;
    private boolean ignoreNotFound;
    private Manipulators.Catcher catcher;
    private Stack<Object[]> stack;
    private List<Object[]> flow = new LinkedList<Object[]>();
    private byte[] hash;

    private Relationship relationship;

    public GraphBuilder() {
        this(true);
    }

    public GraphBuilder(boolean ignoreNotFound) {
        this.ignoreNotFound = ignoreNotFound;
    }

    private int order(){
        return order;
    }

    private void order(Relationship r){
        order(r, order);
    }

    private void order(Relationship r, int order){
        if (order > 0) {
            Order._.add(r, order);
        }
    }

    public Relationship relationship() {
        return relationship;
    };

    private Relationship endGraph() throws AnimoException {
        Relationship r = null;
        Iterator<Object[]> it = flow.iterator();
        if (it.hasNext()) {
            Object[] o = it.next();
            if (o[1] instanceof DEF) {
                if (o[2] == null) {
                    o[2] = MessageDigester.byteArrayToHex(hash);
                }
                Relationship done = DEF._.get(o[2]);
                r = build(getROOT(), o, it, done);
                if (done == null) {
                    Node def = r.getEndNode();
                    NAME.set(def, o[2]);
                    DEFID.set(def, r.getId());
                    DEF._.add(r, o[2]);
                }
                modificative(r);
                preparative(r);
            } else {
                r = Cache.RELATIONSHIP.get(hash);
                if (r == null) {
                    o[6] = Cache.NODE.get(hash) != null;
                    r = build(getROOT(), o, it, null);
                    Cache.RELATIONSHIP.add(r, hash);
                    modificative(r);
                    HASH.set(r, hash);
                    preparative(r);
                }
            }
        }
        return r;
    }

    private Relationship build(Node parent, Object[] o, Iterator<Object[]> it, Relationship r) throws AnimoException {
        if (r == null) {
            r = ((Statement) o[1]).build(parent, o[2], hash, true, ignoreNotFound);
        }
        Node end = r.getEndNode();
        o[3] = r;
        o[4] = end;
        step();
        while (it.hasNext()) {
            build(it.next());
            step();
        }
        return r;
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

    public void start(Statement statement) throws AnimoException, IOException {
        start(statement, null);
    }

    public void start(Object value) throws AnimoException, IOException {
        start(VALUE._, value);
    }

   private Statement s;
   private Object r;

    public void start(Statement statement, Object reference) throws AnimoException, IOException {
        if (statement instanceof DEF) {
            relationship = DEF._.get(reference);
            if (relationship != null && relationship.getEndNode().hasRelationship(OUTGOING)) {
                start(CHANGE._);
                    _(REF._,  reference);
                    start(LINK._);
                return;
            }
        }
    	if (s != null) {
    		stack.push(start(s, r, true));
        }
        s = statement;
        r = reference;
    }

    private Object[] start(Statement statement, Object reference, boolean hasChild) throws AnimoException {
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

    public void end() throws AnimoException, IOException {
    	byte[] hash;
    	if (s != null) {
    		hash = end(start(s, r, false));
    		s = null; r = null;
    	} else {
    		Object[] p = popParent();
    		hash = end(p);
    	}
        if (hasParent()) {
            ((MessageDigest) peekParent()[0]).update(hash);
        }
    }

    private byte[] end(Object[] o) {
        MessageDigest md = (MessageDigest) o[0];
        if (!(Boolean) o[7]) {
            updateMD(md, (Statement) o[1]);
        }
        o[8] = hash = md.digest();
        return hash;
    }

    public void bind(Relationship e) throws IOException, AnimoException {
        Object[] o;
        byte[] hash = HASH.has(e) ? (byte[]) HASH.get(e) : DigestSerializer._.serialize(e);
        if (s != null) {
            o = start(s, r, true);
            stack.push(o);
            bind(e, o, hash);
            s = null; r = null;
        } else {
            o = peekParent();
            bind(e, o, hash);
        }
        if (o != null) {
            ((MessageDigest) o[0]).update(hash);
        }
    };

    public void bind(Relationship r, Object[] p, byte[] hash) throws IOException {
        step();
        Object[] o = {p, r};
        flow.add(o);
    }

    public void _(Statement statement) throws AnimoException, IOException {
        _(statement, null);
    }

    public void _(Object value) throws AnimoException, IOException {
        _(VALUE._, value);
    }

    public void _(Statement statement, Object reference) throws AnimoException, IOException {
        start(statement, reference);
        end();
    }

    private boolean hasParent() {
        return !stack.empty();
    }

    private Object[] popParent() {
        return stack.pop();
    }

    private Object[] peekParent() {
        return stack.peek();
    }

    public void build(AbstractExpression exp) throws Throwable {
        boolean deadlock;
        order = 0;
        catcher = Manipulators.getCatcher();
        do {
            deadlock = false;
            tx = beginTx();
            try {
                s = null; r = null;
                stack = new Stack<Object[]>();
                exp.build();
                while (!stack.empty()) {
                    end();
                }
                Relationship c = endGraph();
                if (relationship == null) {
                    relationship = c;
                } else {
                    evaluative(c);
                }
                tx.success();
                finishTx(tx);
            } catch (Throwable t) {
                finishTx(tx);
                if (t instanceof DeadlockDetectedException) {
                    System.out.println("Deadlock");
                    t.printStackTrace();
                    deadlock = true;
                } else {
                    tx = beginTx();
                    try {
                        catcher.clear();
                    } finally {
                        finishTx(tx);
                    }
                    throw t;
                }
            } finally {
                catcher.push();
            }
        } while (deadlock);
    }

    private void step() {
        order++;
//        if (order % (10000) == 0) {
//            tx.success();
//            finishTx(tx);
//            tx = beginTx();
//        }
    }

    private void evaluative(Relationship r) {
        catcher.evaluative(r);
    }

    private void preparative(Relationship r) {
        catcher.preparative(r);
    }

    private void modificative(Relationship r) {
        catcher.modificative(r);
    }

    private void destructive(Relationship r) {
        catcher.destructive(r);
    }

    private void destructive(Node n) {
        catcher.destructive(n);
    }

}
