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
import org.animotron.statement.value.AbstractValue;
import org.neo4j.graphdb.Relationship;

import java.io.IOException;
import java.io.OutputStream;

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

    @Override
    public void start(Statement statement, Relationship r, int level, boolean isOne) throws IOException {
        if (statement instanceof AbstractValue) {
            Object o = statement.reference(r);
            if (o != null)
                write(o.toString());
        }
    }

    @Override
    public void end(Statement statement, Relationship r, int level, boolean isOne) {
    }

    @Override
    public void start(Statement statement, Object[] param, int level, boolean isOne) throws IOException {
    }

    @Override
    public void end(Statement statement, Object[] param, int level, boolean isOne) throws IOException {
    }

    @Override
    public void start(Statement statement, Object param, int level, boolean isOne) throws IOException {
    }

    @Override
    public void end(Statement statement, Object param, int level, boolean isOne) throws IOException {
    }

    @Override
    public void startGraph() {
    }

    @Override
    public void endGraph() throws IOException {
    }

}