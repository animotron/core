/*
 *  Copyright (C) 2011 The Animo Project
 *  http://animotron.org
 *
 *  This program is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public License
 *  as published by the Free Software Foundation; either version 3
 *  of the License, or (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */
package org.animotron.graph.traverser;

import org.animotron.Properties;
import org.animotron.graph.index.Order;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.index.IndexHits;

import java.util.Iterator;

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
        q = Order.queryDown(n);
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
            if (Properties.VALUE.name().equals(o) || Properties.NAME.name().equals(o)) {
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
        q.close();
    }

}
