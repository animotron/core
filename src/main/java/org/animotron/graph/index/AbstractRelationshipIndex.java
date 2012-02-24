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

import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.index.Index;
import org.neo4j.graphdb.index.RelationshipIndex;

/**
* @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
* @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
*
*/
public abstract class AbstractRelationshipIndex extends AbstractIndex<Relationship>{

    private RelationshipIndex index;

    public AbstractRelationshipIndex(String name){
        super(name);
    }

    @Override
    protected void init(Index<Relationship> index) {
        throw new UnsupportedOperationException();
    }

    protected void init(RelationshipIndex index) {
        this.index = index;
    }

    @Override
    protected RelationshipIndex index() {
        return index;
    }

}
