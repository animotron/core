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
import org.animotron.statement.ml.QNAME;
import org.animotron.statement.operator.DEF;
import org.animotron.statement.operator.REF;
import org.animotron.statement.operator.Utils;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.index.IndexHits;

import java.io.IOException;
import java.util.Iterator;

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
		build(handler, null, r, 0, true, 0, true, true, 0);
		handler.endGraph();
	}
	
    public void traverse(GraphHandler handler, QCAVector vector) throws IOException {
        handler.startGraph();
        build(handler, null, vector, 0, true, 0, true, true, 0);
        handler.endGraph();
    }

	protected void build(GraphHandler handler, Statement parent, Object o, int level, boolean isOne, int pos, boolean isLast, boolean evaluable, long def) throws IOException {
        if (o instanceof Relationship) {
            build(handler, parent, new QCAVector((Relationship)o), level, isOne, pos, isLast, evaluable, def);

        } else if (o instanceof QCAVector) {
            build(handler, parent, (QCAVector)o, level, isOne, pos, isLast, evaluable, def);
            
        } else {
            String name = (String) o;
            Statement statement = Statements.name(name);
            try {
	            String reference = (String) node.getProperty(name);
	            handler.start(statement, parent, reference, level, isOne, pos, isLast);
	            handler.end(statement, parent, reference, level, isOne, pos, isLast);
            } catch (Throwable t) {
			}
        }
    }

	protected void build(GraphHandler handler, Statement parent, QCAVector rr, int level, boolean isOne, int pos, boolean isLast, boolean evaluable, long def) throws IOException {
		Relationship r = rr.getClosest();
		Statement statement = Statements.relationshipType(r);
        if (statement == null)
			return;
		handler.start(statement, parent, r, level++, isOne, pos, isLast);
		if (!(statement instanceof REF)) {
            if (statement instanceof DEF) {
                def = r.getId();
            }
            node = r.getEndNode();
            Relationship ashift = null;
            if (def != 0) {
                ashift = AShift._.get(node, def);
            }
            if (ashift != null) {
                r = getDb().getRelationshipById((Long) RID.get(ashift));
                build(handler, parent, r, level, true, pos++, true, evaluable, def);
            } else {
                iterate(handler, rr, statement, Order._.queryDown(node), level, evaluable, def);
            }
        }
		handler.end(statement, parent, r, --level, isOne, pos, isLast);
	}

    protected void iterate(GraphHandler handler, QCAVector v, Statement parent, IndexHits<Relationship> it, int level, boolean evaluable, long def) throws IOException {

        QCAVector prev;

    	FastTable<Relationship> o = FastTable.newInstance();
        try {
        	Relationship i;

            int count = 0;
            while (it.hasNext()) {
                i = it.next();
                o.add(i);
                if (i instanceof Relationship) {
                    Relationship r = i;
                    if (!(r.isType(REF._) || r.isType(QNAME._))) {
                        break;
                    }
                } else if (!i.equals(QNAME._.name())) {
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
                if (i instanceof Relationship) {
//                	System.out.println(i);
                	prev = new QCAVector(i, v, prev);
                	build(handler, parent, prev, level, isOne, pos++, isLast, evaluable, def);
                } else {
                	build(handler, parent, i, level, isOne, pos++, isLast, evaluable, def);
                }
            }
            
            while (it.hasNext()) {
            	i = it.next();
                if (i instanceof Relationship) {
                	prev = new QCAVector(i, v, prev);
                	build(handler, parent, prev, level, isOne, pos++, !it.hasNext(), evaluable, def);
                } else
                	build(handler, parent, i, level, isOne, pos++, !it.hasNext(), evaluable, def);
            }
        } catch (AnimoException e) {
//        	e.printStackTrace();
        	throw new IOException("on "+v,e);
        } finally {
            FastTable.recycle(o);
            it.close();
        }

    }

}