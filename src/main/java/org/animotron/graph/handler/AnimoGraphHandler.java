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
import org.animotron.statement.link.LINK;
import org.animotron.statement.ml.NAME;
import org.animotron.statement.operator.REF;
import org.animotron.statement.value.VALUE;
import org.neo4j.graphdb.Relationship;

import java.io.IOException;
import java.io.OutputStream;

/**
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class AnimoGraphHandler extends AbstractTextGraphHandler {

    public AnimoGraphHandler(OutputStream stream) {
        super (stream);
    }

    public AnimoGraphHandler(StringBuilder builder) {
        super(builder);
    }

    protected void write(Statement statement, Relationship r) throws IOException {
        write(statement, statement.reference(r));
    }

    protected void write(Statement statement, Object reference) throws IOException {
        if (statement instanceof NAME  && reference != null) {
            write(reference.toString());
        } else if (statement instanceof VALUE && reference != null) {
            write("\"");
            write(reference.toString().replaceAll("\"", "\\\\\""));
            write("\"");
        } else if (statement instanceof REF) {
            write(reference.toString());
        } else if (!(statement instanceof LINK || statement instanceof VALUE)) {
            write(statement.name());
            if (reference != null) {
                write(" ");
                write(reference.toString());
            }
        }
    }

    private Statement ps = null;
    @Override
    public void start(Statement statement, Relationship r, int level, boolean isOne) throws IOException {
        start(statement, statement.reference(r), level, isOne);
    }

    @Override
    public void end(Statement statement, Relationship r, int level, boolean isOne) throws IOException {
        end(statement, level, isOne);
    }

    @Override
    public void start(Statement statement, Object[] param, int level, boolean isOne) throws IOException {
    }

    @Override
    public void end(Statement statement, Object[] param, int level, boolean isOne) throws IOException {
    }

    @Override
    public void start(Statement statement, Object param, int level, boolean isOne) throws IOException {
        if (level != 0 && !(statement instanceof NAME)) {
            if (!(ps instanceof LINK)) {
                write(" ");
            }
            if (!isOne || statement instanceof LINK) {
                write("(");
            }
        }
        write(statement, param);
        ps = statement;
    }

    @Override
    public void end(Statement statement, Object param, int level, boolean isOne) throws IOException {
        end(statement, level, isOne);
    }

    private void end(Statement statement, int level, boolean isOne) throws IOException {
        if (level != 0 && !(statement instanceof NAME) && !isOne || statement instanceof LINK) {
            write(")");
        }
    }

    @Override
    public void startGraph() {
    }

    @Override
    public void endGraph() throws IOException {
    }

}