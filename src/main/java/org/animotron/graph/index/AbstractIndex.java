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
package org.animotron.graph.index;

import org.neo4j.graphdb.PropertyContainer;
import org.neo4j.graphdb.index.Index;
import org.neo4j.graphdb.index.IndexHits;
import org.neo4j.graphdb.index.IndexManager;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public abstract class AbstractIndex<T extends PropertyContainer> {

    protected Index<T> INDEX;
    protected final String name;

    public AbstractIndex(String name){
        this.name = name;
    }

    public abstract void init(IndexManager manager);

    public T get(Object value) {
        IndexHits<T> q = INDEX.get(name, value);
        T c = null;
        try {
            c = q.next();
        } finally {
            q.close();
            return c;
        }
    }

    public void put(T c, Object value) {
        INDEX.add(c, name, value);
    }

    public void remove(T c, Object value) {
        INDEX.remove(c, name, value);
    }

    public void remove(T c) {
        INDEX.remove(c, name);
    }

}
