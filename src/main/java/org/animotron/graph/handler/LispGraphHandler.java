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
import org.animotron.statement.operator.THE;
import org.animotron.statement.relation.IS;
import org.animotron.statement.relation.USE;
import org.neo4j.graphdb.Direction;
import org.neo4j.graphdb.Relationship;

import java.io.IOException;
import java.io.OutputStream;

/**
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class LispGraphHandler extends AbstractTextGraphHandler {

    private int level;

    public LispGraphHandler(OutputStream stream) {
        super(stream);
    }

    public LispGraphHandler(StringBuilder builder) {
        super(builder);
    }

    private boolean haveChild(Statement statement, Relationship r) {
        if (statement instanceof IS || statement instanceof USE || statement instanceof TEXT) {
            return false;
        }
        int n = 0;
        int x = statement instanceof THE ? 1 : 2;
        for (Relationship i : r.getEndNode().getRelationships(Direction.OUTGOING)) {
            n ++;
            if (n > x)
                return true;
        }
        return false;
    }

    @Override
    public void start(Statement statement, Relationship r) throws IOException {
        if (level != 0) {
            write(" ");
            if (haveChild(statement, r)) {
                write("(");
            }
        }
        if (statement instanceof TEXT) {
            write("\"");
            write(statement.value(r));
            write("\"");
        } else {
            write(statement.name());
            String name = statement.name(r);
            if (name != null) {
                write(" ");
                write(statement.name(r));
            }
        }
        level++;
    }

    @Override
    public void end(Statement statement, Relationship r) throws IOException {
        level--;
        if (level != 0 && haveChild(statement, r)) {
            write(")");
        }
    }

    @Override
    public void startGraph() {
        level = 0;
    }

    @Override
    public void endGraph() throws IOException {
        //write("\n");
    }

}