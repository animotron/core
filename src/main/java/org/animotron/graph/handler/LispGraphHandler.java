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
import org.animotron.statement.ml.TEXT;
import org.animotron.statement.operator.AN;
import org.neo4j.graphdb.Relationship;

import java.io.IOException;
import java.io.OutputStream;

/**
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class LispGraphHandler extends AbstractTextGraphHandler {

    public LispGraphHandler(OutputStream stream) {
        super(stream);
    }

    public LispGraphHandler(StringBuilder builder) {
        super(builder);
    }

    @Override
    public void start(Statement statement, Relationship r, int level, boolean isOne) throws IOException {
        if (level != 0) {
            write(" ");
            if (!isOne) {
                write("(");
            }
        }
        if (statement instanceof TEXT) {
            write("\"");
            write(statement.reference(r));
            write("\"");
        } else {
            if (!(statement instanceof AN)) {
                write(statement.name());
                String name = statement.reference(r);
                if (name != null) {
                    write(" ");
                }
            }
            write(statement.reference(r));
        }
    }

    @Override
    public void end(Statement statement, Relationship r, int level, boolean isOne) throws IOException {
        if (level != 0 && !isOne) {
            write(")");
        }
    }

    @Override
    public void startGraph() {
    }

    @Override
    public void endGraph() throws IOException {
        //write("\n");
    }

}