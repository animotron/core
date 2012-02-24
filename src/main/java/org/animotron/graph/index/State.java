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

import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.index.IndexHits;
import org.neo4j.graphdb.index.IndexManager;
import org.neo4j.index.bdbje.BerkeleyDbIndexImplementation;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public enum State {

    GC      (0),
    TOP     (1),
    PREPARE (2),
    CALC    (3),
    REV     (4);

	public static final AbstractIndex<Node> _ = new AbstractIndex<Node>("state") {
        @Override
        public void init(IndexManager index) {
            init(index.forNodes(name, BerkeleyDbIndexImplementation.DEFAULT_CONFIG));
        }
    };

    private int id;

    private State(int id) {
         this.id = id;
    }

    public void add(Node n) {
        _.add(n, id);
    }
	
    public void remove(Node n) {
        _.remove(n, id);
    }

    public IndexHits<Node> getHits() {
        return _.getHits(id);
    }

}