/*
 *  Copyright (C) 2011-2012 The Animo Project
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

import org.animotron.manipulator.Controller;
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
    
    public Controller getController() {
    	return null;
    }

    @Override
    public void start(Statement statement, Statement parent, Relationship r, int level, boolean isOne, int pos, boolean isLast) throws IOException {
        MessageDigest md = MessageDigester.md();
        updateMD(md, statement.reference(r));
        stack.push(md);
    }

    @Override
    public void end(Statement statement, Statement parent, Relationship r, int level, boolean isOne, int pos, boolean isLast) throws IOException {
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