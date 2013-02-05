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
package org.animotron.graph.serializer;

import org.neo4j.graphdb.Relationship;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;

import static org.animotron.graph.AnimoGraph.getROOT;
import static org.animotron.graph.Properties.HASH;
import static org.animotron.utils.MessageDigester.byteArrayToHex;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class DBAnimoSerializer {

    public static void serialize(File folder) throws IOException {
        for (Relationship r : getROOT().getRelationships()) {
            String hash = byteArrayToHex((byte[]) HASH.get(r));
            File f = new File(new File (folder, hash.substring(0, 1)), hash.substring(0, 3));
            f.mkdirs();
            Serializer.PRETTY_ANIMO.serialize(r, new FileOutputStream(new File(f, hash)));
        }
    }

}