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
import org.animotron.expression.Expression;
import org.animotron.graph.AnimoGraph;
import org.animotron.manipulator.Manipulators;
import org.animotron.statement.Statement;
import org.animotron.statement.value.VALUE;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.Transaction;

import static org.animotron.graph.AnimoGraph.beginTx;
import static org.animotron.graph.AnimoGraph.finishTx;

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
    public final Manipulators.Catcher catcher;

    public GraphBuilder() {
        this(true);
    }

    public GraphBuilder(boolean ignoreNotFound) {
        this.ignoreNotFound = ignoreNotFound;
        catcher = Manipulators.getCatcher();
    }

    protected final int order(){
        return order;
    }

    protected final void order(Relationship r){
        order(r, order);
    }

    protected final void order(Relationship r, int order){
        if (order > 0) {
            AnimoGraph.order(r, order);
        }
    }

    public abstract Relationship relationship();

    protected abstract void fail(Exception e);

    public abstract void startGraph();

    public abstract void endGraph() throws AnimoException;

    final public void start(Statement statement) throws AnimoException {
        start(statement, null);
    };

    final public void start(Object value) throws AnimoException {
        start(VALUE._, value);
    }

	public abstract void start(Statement statement, Object reference) throws AnimoException;

	public abstract void end() throws AnimoException;

    final public void build(Expression exp) throws Exception {
        order = 0;
        tx = beginTx();
        try {
            exp.build();
            tx.success();
            finishTx(tx);
        } catch (Exception e) {
            finishTx(tx);
            tx = beginTx();
            try {
                fail(e);
            } finally {
                finishTx(tx);
            }
            throw e;
        } finally {
            catcher.push();
        }
    }

    final protected void step() {
        if (order % (30000) == 0) {
            tx.success();
            finishTx(tx);
            tx = beginTx();
        }
        order++;
        if (order % 1000 == 0) {
            System.out.println(order);
        }
    }

}
