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

import org.animotron.statement.Statement;
import org.animotron.statement.instruction.ml.TEXT;
import org.neo4j.graphdb.Relationship;

import java.io.IOException;
import java.io.OutputStream;

/**
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class TextGraphHandler implements GraphHandler {

    Handler out;

    public TextGraphHandler (OutputStream stream) {
        out = new StreamHandler(stream);
    }

    public TextGraphHandler (StringBuilder builder) {
        out = new StringHandler(builder);
    }

    @Override
    public void start(Statement statement, Relationship r) throws IOException {
        if (statement instanceof TEXT) {
            out.write(statement.value(r));
        }
    }

    @Override
    public void end(Statement statement, Relationship r) {
    }

    @Override
    public void startGraph() {
    }

    @Override
    public void endGraph() {
    }

    private interface Handler {
        public void write(String string) throws IOException;
    }

    private class StreamHandler implements Handler {
        OutputStream out;
        public StreamHandler(OutputStream stream) {
            out = stream;
        }
        public void write(String s) throws IOException {
            out.write(s.getBytes());
        }
    }

    private class StringHandler implements Handler {
        StringBuilder out;
        public StringHandler(StringBuilder builder) {
            out = builder;
        }
        public void write(String s) {
            out.append(s);
        }
    }
}