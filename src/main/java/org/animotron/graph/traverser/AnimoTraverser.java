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
package org.animotron.graph.traverser;

import javolution.util.FastTable;
import org.animotron.exception.AnimoException;
import org.animotron.graph.handler.GraphHandler;
import org.animotron.graph.index.AShift;
import org.animotron.graph.index.Order;
import org.animotron.manipulator.QCAVector;
import org.animotron.statement.Statement;
import org.animotron.statement.Statements;
import org.animotron.statement.link.LINK;
import org.animotron.statement.ml.QNAME;
import org.animotron.statement.operator.DEF;
import org.animotron.statement.operator.REF;
import org.animotron.statement.operator.Reference;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.index.IndexHits;

import java.io.IOException;

import static org.animotron.graph.AnimoGraph.getDb;
import static org.animotron.graph.Properties.RID;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovskyd@gmail.com">Evgeny Gazdovsky</a>
 * 
 */
public class AnimoTraverser {

    public static AnimoTraverser _ = new AnimoTraverser();
    protected Node node;

    protected AnimoTraverser() {}
	
	public void traverse(GraphHandler handler, Relationship r) throws IOException {
		handler.startGraph();
		build(handler, null, r, 0, true, 0, true, true, null);
		handler.endGraph();
	}
	
    public void traverse(GraphHandler handler, QCAVector vector) throws IOException {
        handler.startGraph();
        build(handler, null, vector, 0, true, 0, true, true, null);
        handler.endGraph();
    }

    protected void build(GraphHandler handler, Statement parent, Relationship r, int level, boolean isOne, int pos, boolean isLast, boolean evaluable, Relationship def) throws IOException {
        build(handler, parent, new QCAVector(def == null? -1 : def.getId(), r), level, isOne, pos, isLast, evaluable, def);
    }

    protected void build(GraphHandler handler, Statement parent, QCAVector rr, int level, boolean isOne, int pos, boolean isLast, boolean evaluable, Relationship def) throws IOException {
		Relationship r = rr.getClosest();
		Statement statement = Statements.relationshipType(r);
        if (statement == null)
			return;
		handler.start(statement, parent, r, level++, isOne, pos, isLast);
        if (!(statement instanceof REF)) {
            iterate(statement, handler, parent, rr, r, level, pos, evaluable,def);
        }
		handler.end(statement, parent, r, --level, isOne, pos, isLast);
	}

    protected void iterate(Statement statement, GraphHandler handler, Statement parent, QCAVector rr, Relationship r, int level, int pos, boolean evaluable, Relationship def) throws IOException {
        node = r.getEndNode();
        if (statement instanceof DEF) {
            def = r;
        } else if (statement instanceof REF) {
        	//XXX: make it cheaper!
            def = DEF.getRelationship(r.getEndNode());
        }
        Relationship ashift = null;
        if (def != null) {
            ashift = AShift._.get(node, def.getId());
        }
        if (ashift != null) {
            ashift = getDb().getRelationshipById((Long) RID.get(ashift));
            pos = statement instanceof Reference ? iterateRef(handler, rr, statement, node, level, evaluable, def) : 0;
            if (ashift.isType(LINK._)) {
                iterate(handler, rr, statement, ashift, level, evaluable, def);
            } else {
                build(handler, parent, ashift, level, true, pos++, true, evaluable, def);
            }
        } else {
            iterate(handler, rr, statement, node, level, evaluable, def);
        }
    }

    private int iterateRef(GraphHandler handler, QCAVector v, Statement parent, Node n, int level, boolean evaluable, Relationship def) throws IOException {
        QCAVector prev;
        FastTable<Relationship> o = FastTable.newInstance();
        IndexHits<Relationship> it = Order._.queryDown(n);
        try {
            int count = 0;
            while (it.hasNext()) {
                Relationship i = it.next();
                if (!(i.isType(REF._) || i.isType(QNAME._))) {
                    if (count == 0) {
                        o.add(i);
                    }
                    break;
                }
                o.add(i);
                count++;
            }
            prev = null;
            int pos = 0;
            boolean isOne = o.size() - count < 2;
            for (int index = 0, size = o.size(); index < size; index++) {
                Relationship i = o.get(index);
                boolean isLast = pos < o.size() - 1 ? false : !it.hasNext();
                prev = new QCAVector(i, v, prev);
                build(handler, parent, prev, level, isOne, pos++, isLast, evaluable, def);
            }
            return pos;
        } catch (AnimoException e) {
//        	e.printStackTrace();
            throw new IOException("on "+v,e);
        } finally {
            FastTable.recycle(o);
            it.close();
        }

    }

    protected void iterate(GraphHandler handler, QCAVector v, Statement parent, Relationship r, int level, boolean evaluable, Relationship def) throws IOException {
        iterate(handler, v, parent, r.getEndNode(), level, evaluable, def);
    }

    protected void iterate(GraphHandler handler, QCAVector v, Statement parent, Node n, int level, boolean evaluable, Relationship def) throws IOException {
        iterate(handler, v, parent, Order._.queryDown(n), level, evaluable, def);
    }

    protected void iterate(GraphHandler handler, QCAVector v, Statement parent, IndexHits<Relationship> it, int level, boolean evaluable, Relationship def) throws IOException {
       QCAVector prev;
    	FastTable<Relationship> o = FastTable.newInstance();
        try {
        	Relationship i;
            int count = 0;
            while (it.hasNext()) {
                i = it.next();
                o.add(i);
                Relationship r = i;
                if (!(r.isType(REF._) || r.isType(QNAME._))) {
                    break;
                }
                count++;
            }
            int n = 0;
            while (it.hasNext() && n < 2) {
                o.add(it.next());
                n++;
            }
            prev = null;
            int pos = 0;
            boolean isOne = o.size() - count < 2;
        	for (int index = 0, size = o.size(); index < size; index++) {
        		i = o.get(index);
                boolean isLast = pos < o.size() - 1 ? false : !it.hasNext();
                prev = new QCAVector(i, v, prev);
                build(handler, parent, prev, level, isOne, pos++, isLast, evaluable, def);
            }
            while (it.hasNext()) {
            	i = it.next();
                prev = new QCAVector(i, v, prev);
                build(handler, parent, prev, level, isOne, pos++, !it.hasNext(), evaluable, def);
            }
        } catch (AnimoException e) {
        	e.printStackTrace();
        	throw new IOException("on "+v,e);
        } finally {
            FastTable.recycle(o);
            it.close();
        }

    }

}