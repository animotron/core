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
package org.animotron.graph.serializer;

import org.animotron.graph.AnimoGraph;
import org.animotron.graph.GraphOperation;
import org.animotron.graph.handler.DigestGraphHandler;
import org.animotron.graph.traverser.AnimoTraverser;
import org.neo4j.graphdb.Relationship;

import java.io.IOException;

import static org.animotron.graph.Properties.HASH;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class DigestSerializer {

    public static byte[] serialize(final Relationship r) throws IOException {
        if (HASH.has(r)) {
            return (byte[]) HASH.get(r);
        }
        DigestGraphHandler handler = new DigestGraphHandler();
        AnimoTraverser._.traverse(handler, r);
        final byte[] hash = handler.digest();
        try {
            return AnimoGraph.execute(new GraphOperation<byte[]>() {
                @Override
                public byte[] execute() throws Throwable {
                    HASH.set(r, hash);
                    return hash;
                }
            });
        } catch (Throwable t) {
            throw new IOException(t);
        }
    }

}