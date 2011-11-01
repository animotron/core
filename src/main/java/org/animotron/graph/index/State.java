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
package org.animotron.graph.index;

import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.index.Index;
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
    CALC    (3);
	
	private static final String NAME = "state";
	
	private static Index<Node> INDEX;
    private int id;

    private State(int id) {
         this.id = id;
    }

    public static void init(IndexManager indexManager) {
        INDEX = indexManager.forNodes(NAME, BerkeleyDbIndexImplementation.DEFAULT_CONFIG);
	}

    public void add(Node n) {
        INDEX.add(n, NAME, id);
    }
	
    public void remove(Node n) {
        INDEX.remove(n, NAME, id);
    }

    public IndexHits<Node> get() {
        return INDEX.get(NAME, id);
    }

}