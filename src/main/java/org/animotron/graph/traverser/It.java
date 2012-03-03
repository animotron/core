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

import org.animotron.graph.index.Order;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.index.IndexHits;

import java.util.Iterator;

import static org.animotron.graph.Properties.*;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovskyd@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class It implements Iterator<Object>, Iterable<Object> {

    private Iterator<String> p;
    private Iterator<Relationship> r;
    private IndexHits<Relationship> q;

    Object current = null;

    public It(Relationship r) {
        this(r.getEndNode());
    }

    public It(Node n) {
        p = n.getPropertyKeys().iterator();
        q = Order._.queryDown(n);
        r = q.iterator();
        next();
    }

    @Override
    public Iterator<Object> iterator() {
        return this;
    }

    @Override
    public boolean hasNext() {
        return current != null;
    }

    @Override
    public Object next() {
        Object next = current;
        current = step();
        return next;
    }

    private Object step() {
        if (p.hasNext()) {
            String o = p.next();
            if (VALUE.equals(o) ||
                    NAME.equals(o) ||
                        CONTEXT.equals(o) ||
                            MODIFIED.equals(o) ||
                                CACHE.equals(o) ||
                                    UUID.equals(o) ||
                                        FREEZE.equals(o) ||
                                            THEID.equals(o) ||
                                                ARID.equals(o)) {
                return step();
            } else {
                return o;
            }
        } else if (r.hasNext()) {
            return r.next();
        } else {
            return null;
        }
    }

    @Override
    public void remove() {
    	//XXX: remove
        q.close();
    }
    
    public void close() {
        q.close();
    }
}
