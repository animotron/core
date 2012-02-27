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
import org.animotron.graph.index.Order;
import org.animotron.graph.serializer.DigestSerializer;
import org.animotron.manipulator.Manipulators;
import org.animotron.statement.Statement;
import org.animotron.statement.value.VALUE;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.Transaction;

import java.io.IOException;
import java.security.MessageDigest;
import java.util.Stack;

import static org.animotron.graph.AnimoGraph.beginTx;
import static org.animotron.graph.AnimoGraph.finishTx;
import static org.animotron.graph.Properties.HASH;

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
public abstract class GraphBuilder {

    Transaction tx;
    private int order;
    protected final boolean ignoreNotFound;
    private Manipulators.Catcher catcher;
    private Stack<Object[]> stack;

    public GraphBuilder() {
        this(true);
    }

    public GraphBuilder(boolean ignoreNotFound) {
        this.ignoreNotFound = ignoreNotFound;
    }

    protected final int order(){
        return order;
    }

    protected final void order(Relationship r){
        order(r, order);
    }

    protected final void order(Relationship r, int order){
        if (order > 0) {
            Order._.add(r, order);
        }
    }

    public abstract Relationship relationship();

    protected abstract void fail(Throwable t);

    protected abstract void startGraph();

    protected abstract void endGraph() throws AnimoException, IOException;

    public final void start(Statement statement) throws AnimoException, IOException {
        start(statement, null);
    }

    public final void start(Object value) throws AnimoException, IOException {
        start(VALUE._, value);
    }

    Statement s; Object r;

    public final void start(Statement statement, Object reference) throws AnimoException, IOException {
    	if (s != null) {
    		stack.push(start(s, r, true));
        }
        s = statement;
        r = reference;
    }

    protected abstract Object[] start(Statement statement, Object reference, boolean ready) throws AnimoException, IOException;

    public final void end() throws AnimoException, IOException {
    	byte[] hash;
    	if (s != null) {
    		hash = end(start(s, r, false), false);
    		s = null; r = null;
    	} else {
    		Object[] p = popParent();
    		hash = end(p, true);
    	}
        if (hasParent()) {
            ((MessageDigest) peekParent()[0]).update(hash);
        }
    }

	protected abstract byte[] end(Object[] o, boolean hasChild) throws AnimoException;

    public final void bind(Relationship e) throws IOException, AnimoException {
        Object[] o;
        byte[] hash = HASH.has(e) ? (byte[]) HASH.get(e) : DigestSerializer._.serialize(e);
        if (s != null) {
            o = start(s, r, true);
            stack.push(o);
            bind(e, o, hash);
            s = null; r = null;
        } else {
            o = peekParent();
            bind(e, peekParent(), hash);
        }
        if (o != null) {
            ((MessageDigest) o[0]).update(hash);
        }
    };

    protected abstract void bind(Relationship r, Object[] o, byte[] hash) throws IOException;

    public final void _(Statement statement) throws AnimoException, IOException {
        _(statement, null);
    }

    public final void _(Object value) throws AnimoException, IOException {
        _(VALUE._, value);
    }

    public final void _(Statement statement, Object reference) throws AnimoException, IOException {
        start(statement, reference);
        end();
    }

    protected final boolean hasParent() {
        return !stack.empty();
    }

    protected final Object[] popParent() {
        return stack.pop();
    }

    protected final Object[] peekParent() {
        return stack.peek();
    }

    public final void build(AbstractExpression exp) throws Throwable {
        order = 0;
        catcher = Manipulators.getCatcher();
        tx = beginTx();
        try {
        	s = null; r = null;
            stack = new Stack<Object[]>();
            startGraph();
            exp.build();
        	endGraph();
            tx.success();
            finishTx(tx);
        } catch (Throwable t) {
            finishTx(tx);
            tx = beginTx();
            try {
                preparative(null);
                modified(null);
                fail(t);
            } finally {
                finishTx(tx);
            }
            throw t;
        } finally {
            catcher.push();
        }
    }

    protected final void step() throws IOException {
        order++;
        if (order % (10000) == 0) {
            tx.success();
            finishTx(tx);
            tx = beginTx();
        }
    }

    protected final void preparative(Relationship r) {
        catcher.preparative(r);
    }

    protected final void modified(Relationship r) {
        catcher.modified(r);
    }

    protected final void destructive(Relationship r) {
        catcher.destructive(r);
    }

    protected final void destructive(Node n) {
        catcher.destructive(n);
    }

}
