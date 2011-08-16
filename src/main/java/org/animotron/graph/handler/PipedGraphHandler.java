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
package org.animotron.graph.handler;

import org.animotron.Statement;
import org.animotron.instruction.ml.TEXT;
import org.animotron.io.PipedOutput;
import org.neo4j.graphdb.Relationship;

import java.io.IOException;

/**
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class PipedGraphHandler implements GraphHandler {

    PipedOutput pipe;

    public PipedGraphHandler(PipedOutput pipe) {
        this.pipe = pipe;
    }

    @Override
    public void start(Statement statement, Relationship r) throws InterruptedException {
        if (statement instanceof TEXT) {
            try {
                pipe.write(statement);
                pipe.write(r);
            } catch (IOException e) {
                throw new InterruptedException(e.getMessage());
            }
        }
    }

    @Override
    public void end(Statement statement, Relationship r) {
    }

    @Override
    public void startGraph() {
    }

    @Override
    public void endGraph() throws InterruptedException {
        try {
            pipe.close();
        } catch (IOException e) {
            throw new InterruptedException(e.getMessage());
        }
    }

}