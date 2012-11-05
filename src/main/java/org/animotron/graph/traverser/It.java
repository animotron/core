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

import org.animotron.graph.AnimoGraph;
import org.animotron.graph.index.AShift;
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
public class It implements Iterator<Relationship>, Iterable<Relationship> {

    private Iterator<Relationship> r = null;
    private IndexHits<Relationship> q = null;

    Relationship current = null;

    public It(Node n) {
        q = Order._.queryDown(n);
        r = q.iterator();
        next();
    }

    public It(Node n, long def) {
        Relationship ashift = AShift._.get(n, def);
        if (ashift != null) {
            q = AShift._.queryDown(n, def);
        } else {
            q = Order._.queryDown(n);
        }
        r = q.iterator();
        next();
    }

    @Override
    public Iterator<Relationship> iterator() {
        return this;
    }

    @Override
    public boolean hasNext() {
        return current != null;
    }

    @Override
    public Relationship next() {
        Relationship next = current;
        current = step();
        return next;
    }

    private Relationship step() {
        if (r.hasNext()) {
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
