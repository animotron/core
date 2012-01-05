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

import org.animotron.graph.Properties;
import org.animotron.statement.Statement;
import org.animotron.statement.value.STREAM;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;

import java.io.*;

/**
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class BinaryGraphHandler implements GraphHandler {

    OutputStream out;

    public BinaryGraphHandler(OutputStream out) {
        this.out = out;
    }

    protected void write(Node n, OutputStream out) throws IOException {
        InputStream in = new FileInputStream(new File((String) Properties.VALUE.get(n)));
        byte buf[] = new byte[1024 * 4];
        int len;
        while((len=in.read(buf))>0) {
            out.write(buf, 0, len);
        }
    }

    @Override
    public void start(Statement statement, Statement parent, Relationship r, int level, boolean isOne, int pos, boolean isLast) throws IOException {
        if (statement instanceof STREAM) {
            write(r.getEndNode(), out);
        }
    }

    @Override
    public void end(Statement statement, Statement parent, Relationship r, int level, boolean isOne, int pos, boolean isLast) {
    }

    @Override
    public void start(Statement statement, Statement parent, Object[] param, int level, boolean isOne, int pos, boolean isLast) throws IOException {
    }

    @Override
    public void end(Statement statement, Statement parent, Object[] param, int level, boolean isOne, int pos, boolean isLast) throws IOException {
    }

    @Override
    public void start(Statement statement, Statement parent, Object param, int level, boolean isOne, int pos, boolean isLast) throws IOException {
    }

    @Override
    public void end(Statement statement, Statement parent, Object param, int level, boolean isOne, int pos, boolean isLast) throws IOException {
    }

    @Override
    public void startGraph() {
    }

    @Override
    public void endGraph() {
    }

	private boolean stepMade = false;
	
	public void stepMade() {
		stepMade = true;
	}

	public boolean isStepMade() {
		return stepMade;
	}
}