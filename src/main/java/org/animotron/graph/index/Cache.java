/*
 *  Copyright (C) 2011-2013 The Animo Project
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
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.index.IndexManager;
import org.neo4j.index.bdbje.BerkeleyDbIndexImplementation;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class Cache {

	private static final String NAME = "cache";

    public static AbstractIndex<Node> NODE = new AbstractIndex<Node>(NAME) {
        @Override
        public void init(IndexManager index) {
            init(index.forNodes(name, BerkeleyDbIndexImplementation.DEFAULT_CONFIG));
        }
    };

    public static AbstractIndex<Relationship> RELATIONSHIP= new AbstractIndex<Relationship>(NAME) {
        @Override
        public void init(IndexManager index) {
            init(index.forRelationships(name, BerkeleyDbIndexImplementation.DEFAULT_CONFIG));
        }
    };

}