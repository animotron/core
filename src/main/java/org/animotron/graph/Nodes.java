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
package org.animotron.graph;

import org.animotron.statement.operator.THE;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Transaction;


/**
 * @author <a href="mailto:gazdovskyd@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class Nodes {

    public final static Node EXTENSION;
    public final static Node MIME_TYPE;
    public final static Node TYPE;
    public final static Node FILE;
    public final static Node NAME;
    public final static Node URI;
    
    static {
    	Transaction tx = AnimoGraph.beginTx();
    	try {
	        EXTENSION = THE._("extension");
	        MIME_TYPE = THE._("mime-type");
	        TYPE = THE._("type");
	        FILE = THE._("file");
	        NAME = THE._("name");
	        URI = THE._("uri");
	        
	        tx.success();
    	} finally {
    		AnimoGraph.finishTx(tx);
    	}
    }

}
