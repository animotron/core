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
package org.animotron.synchro;

import org.animotron.graph.handler.GraphHandler;
import org.animotron.graph.serializer.DigestSerializer;
import org.animotron.graph.traverser.AnimoTraverser;
import org.animotron.statement.operator.DEF;
import org.neo4j.graphdb.Relationship;

import java.io.IOException;
import java.io.OutputStream;

import static org.animotron.synchro.StreamUtils.*;

/**
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class Protocol {

    public static void writeDefVersion(Relationship def, OutputStream os) throws IOException {
        os.write(START_GRAPH);
        writeBytes(os, DigestSerializer.serialize(def));
        writeString(os, (String) DEF._.reference(def));
        os.write(StreamUtils.END_GRAPH);
    }

    public static void writeExpression(Relationship r, OutputStream os) throws IOException {
        GraphHandler handler = new BinaryGraphHandler(os);
        AnimoTraverser._.traverse(handler, r);
    }

}