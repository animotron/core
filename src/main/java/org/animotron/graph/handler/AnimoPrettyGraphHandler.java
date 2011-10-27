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
import org.animotron.statement.ml.Prefix;
import org.neo4j.graphdb.Relationship;

import java.io.IOException;
import java.io.OutputStream;
import java.util.LinkedList;
import java.util.List;
import java.util.Stack;

/**
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class AnimoPrettyGraphHandler extends AnimoGraphHandler {

    private Stack<Object[]> stack;
    private final static String INDENT = "    ";

    public AnimoPrettyGraphHandler(OutputStream stream) {
        super (stream);
    }

    public AnimoPrettyGraphHandler(StringBuilder builder) {
        super(builder);
    }

    @Override
    public void startGraph() {
        stack = new Stack<Object[]>();
    }

    @Override
    public void start(Statement statement, Relationship r, int level, boolean isOne) throws IOException {
        Object[] item = {statement, r, level, isOne, new LinkedList<Object[]>(), !isOne};
        if (!stack.empty()) {
            Object[]p = stack.peek();
            ((List<Object[]>) p[4]).add(item);
        }
        stack.push(item);
    }

    private Object[] root;

    @Override
    public void end(Statement statement, Relationship r, int level, boolean isOne) throws IOException {
        root = stack.pop();
        int size = ((List<Object[]>)root[4]).size();
        if (statement instanceof Prefix) size--;
        root[5] = (Boolean)root[5] || size > 1 || level == 1;
    }

    @Override
    public void endGraph() throws IOException {
        write(root, 0);
        write("\n");
    }

    private Statement ps = null;
    private void write(Object[] o, int indent) throws IOException {
        Statement statement = (Statement) o[0];
        int level = (Integer) o[2];
        boolean isOne = (Boolean) o[3];
        if (level != 0 && !(statement instanceof NAME)) {
            if ((Boolean) o[5]) {
                indent++;
                write("\n");
                for (int i = 0; i < indent; i++) {
                    write(INDENT);
                }
            } else if (!(ps instanceof LINK)) {
                write(" ");
            }
            if (!isOne || statement instanceof LINK) {
                write("(");
            }
        }
        write(statement, (Relationship) o[1]);
        for (Object[] i : (List<Object[]>) o[4]) {
            write(i, indent);
        }
        if (level != 0 && !(statement instanceof NAME) && !isOne || statement instanceof LINK) {
            write(")");
        }
        ps = statement;
    }

}