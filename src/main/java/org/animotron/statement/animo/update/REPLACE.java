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
package org.animotron.statement.animo.update;

import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Path;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 */
public class REPLACE extends AbstractUpdate {

    public static final REPLACE _ = new REPLACE();

	private REPLACE() {super("replace");}

//    @Override
//    protected void execute(Set<Relationship> the, Relationship destination, Set<Relationship> target) throws IOException {
//        for (Relationship r: the) {
//            if (r.getEndNode().equals(destination.getEndNode())) {
//                throw new UnsupportedOperationException();
//            } else {
//                //TODO : implement
//            }
//        }
//    }

    @Override
    protected Node process(Node rev, Node x) {
        //To change body of implemented methods use File | Settings | File Templates.
    }

    @Override
    protected Node process(Node rev, Node x, Path diff) {
        //To change body of implemented methods use File | Settings | File Templates.
    }

}