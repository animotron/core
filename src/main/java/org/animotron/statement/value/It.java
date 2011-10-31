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
 *  but WITHOUT ALL WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */
package org.animotron.statement.value;

import org.animotron.statement.Statement;
import org.animotron.statement.Statements;
import org.neo4j.graphdb.Node;

import java.util.Iterator;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class It implements Iterable<Object[]>, Iterator<Object[]> {

    private Iterator<String> it;
    Object[] current = null;
    private Node n;

    public It(Node n) {
        this.n = n;
        this.it = n.getPropertyKeys().iterator();
        next();
    }

    @Override
    public Iterator<Object[]> iterator() {
        return this;
    }

    @Override
    public boolean hasNext() {
        return current != null;
    }

    @Override
    public Object[] next() {
        Object[] next = current;
        current = step();
        return next;
    }

    private Object[] step() {
        if (it.hasNext()) {
            String name = it.next();
            Statement s = Statements.name(name);
            if (s == null) {
                return step();
            }
            Object [] o = {s, n.getProperty(name)};
            return o;
        }
        return null;
    }

    @Override
    public void remove() {
        it.remove();
    }

}
