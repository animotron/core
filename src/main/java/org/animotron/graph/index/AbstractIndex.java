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
    protected final String name;
    private Index<T> index;

    public AbstractIndex(String name){
        this.name = name;
    }

    protected Index<T> index() {
        return index;
    };

    protected void init(Index<T> index) {
        this.index = index;
    };

    public abstract void init(IndexManager index);

    public T get(Object value) {
        IndexHits<T> q = index().get(name, value);
        T c = null;
        try {
            c = q.next();
        } finally {
            q.close();
            return c;
        }
    }

    public IndexHits<T> getHits(Object value) {
        return index().get(name, value);
    }

    public void add(T c, Object value) {
        index().add(c, name, value);
    }

    public void remove(T c, Object value) {
        index().remove(c, name, value);
    }

    public void remove(T c) {
        index().remove(c, name);
    }

}
