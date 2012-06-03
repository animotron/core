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
import org.animotron.manipulator.QCAVector;
import org.animotron.statement.Statement;
import org.animotron.statement.Statements;
import org.animotron.statement.ml.QNAME;
import org.animotron.statement.operator.DEF;
import org.animotron.statement.operator.REF;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;

import java.io.IOException;

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
		build(handler, null, r, 0, true, 0, true);
		handler.endGraph();
	}
	
    public void traverse(GraphHandler handler, QCAVector vector) throws IOException {
        handler.startGraph();
        build(handler, null, vector, 0, true, 0, true);
        handler.endGraph();
    }

	protected void build(GraphHandler handler, Statement parent, Object o, int level, boolean isOne, int pos, boolean isLast) throws IOException {
        if (o instanceof Relationship) {
            build(handler, parent, new QCAVector((Relationship)o), level, isOne, pos, isLast);

        } else if (o instanceof QCAVector) {
            build(handler, parent, (QCAVector)o, level, isOne, pos, isLast);
            
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

	protected void build(GraphHandler handler, Statement parent, QCAVector rr, int level, boolean isOne, int pos, boolean isLast) throws IOException {
		Relationship r = rr.getClosest();
		Statement statement = Statements.relationshipType(r);
		if (statement == null)
			return;
		handler.start(statement, parent, r, level++, isOne, pos, isLast);
		if (!(statement instanceof REF)) {
            node = (statement instanceof DEF ? DEF._.getActualRevision(r) : r.getEndNode());
            It it = new It(node);
            iterate(handler, rr, statement, it, level);
		}
		handler.end(statement, parent, r, --level, isOne, pos, isLast);
	}

    protected void iterate(GraphHandler handler, QCAVector v, Statement parent, It it, int level) throws IOException {
        QCAVector prev = null;

    	FastTable<Object> o = FastTable.newInstance();
        try {
        	Object i = null;

            int count = 0;
            while (it.hasNext()) {
                i = it.next();
                o.add(i);
                if (i instanceof Relationship) {
                    Relationship r = (Relationship) i;
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
                	prev = new QCAVector((Relationship)i, v, prev);
                	build(handler, parent, prev, level, isOne, pos++, isLast);
                } else {
                	build(handler, parent, i, level, isOne, pos++, isLast);
                }
            }
            
            while (it.hasNext()) {
            	i = it.next();
                if (i instanceof Relationship) {
                	prev = new QCAVector((Relationship)i, v, prev);
                	build(handler, parent, prev, level, isOne, pos++, !it.hasNext());
                } else
                	build(handler, parent, i, level, isOne, pos++, !it.hasNext());
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