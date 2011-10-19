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
import org.animotron.utils.MessageDigester;
import org.neo4j.graphdb.Relationship;

import java.io.IOException;
import java.security.MessageDigest;
import java.util.Stack;

import static org.animotron.utils.MessageDigester.updateMD;

/**
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class DigestGraphHandler implements GraphHandler {

    private MessageDigest md;
    private Stack<MessageDigest> stack;

    @Override
    public void start(Statement statement, Relationship r, int level, boolean isOne) throws IOException {
        start(statement, statement.reference(r), level, isOne);
    }

    @Override
    public void end(Statement statement, Relationship r, int level, boolean isOne) throws IOException {
        end(statement, statement.reference(r), level, isOne);
    }

    @Override
    public void start(Statement statement, Object[] param, int level, boolean isOne) throws IOException {
        start(statement, (Object) param, level, isOne);
    }

    @Override
    public void end(Statement statement, Object[] param, int level, boolean isOne) throws IOException {
        end(statement, (Object) param, level, isOne);
    }

    @Override
    public void start(Statement statement, Object param, int level, boolean isOne) throws IOException {
        MessageDigest md = MessageDigester.md();
        updateMD(md, param);
        stack.push(md);
    }

    @Override
    public void end(Statement statement, Object param, int level, boolean isOne) throws IOException {
        md = stack.pop();
        updateMD(md, statement);
        if (!stack.empty()) {
            stack.peek().update(md.digest());
        }
    }

    @Override
    public void startGraph() {
        stack = new Stack<MessageDigest>();
    }

    @Override
    public void endGraph() throws IOException {
    }

    public byte[] digest() {
        return md.digest();
    }

}