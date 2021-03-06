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
package org.animotron.graph.handler;

import org.animotron.statement.Statement;
import org.animotron.statement.operator.VALUE;
import org.neo4j.graphdb.Relationship;

import java.io.IOException;
import java.io.OutputStream;
import java.io.Writer;

/**
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class TextGraphHandler extends AbstractTextGraphHandler {

    public TextGraphHandler (OutputStream stream) {
        super(stream);
    }

    public TextGraphHandler (StringBuilder builder) {
        super(builder);
    }

    public TextGraphHandler(Writer writer) {
        super(writer);
    }

    @Override
    public void start(Statement statement, Statement parent, Relationship r, int level, boolean isOne, int pos, boolean isLast) throws IOException {
        if (statement instanceof VALUE) {
            Object o = statement.reference(r);
            if (o != null)
                write(o.toString());
        }
    }

    @Override
    public void end(Statement statement, Statement parent, Relationship r, int level, boolean isOne, int pos, boolean isLast) {
    }

    @Override
    public void startGraph() {
        if (controller != null)
        	controller.start();
    }

    @Override
    public void endGraph() throws IOException {
        if (controller != null)
        	controller.end();
    }
}