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

import org.animotron.Properties;
import org.animotron.Statement;
import org.animotron.graph.builder.BinaryBuilder;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;

import java.io.*;

import static org.animotron.Properties.BIN;

/**
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class BinaryGraphHandler implements GraphHandler {

    OutputStream out;

    public BinaryGraphHandler(OutputStream out) {
        this.out = out;
    }

    @Override
    public void start(Statement statement, Relationship r) throws InterruptedException {
        Node n = r.getEndNode();
        if (BIN.has(n)) {
            try {
                File bin = BinaryBuilder.getFile(Properties.BIN.get(n));
                InputStream in = null;
                    in = new FileInputStream(bin);
                byte buf[] = new byte[1024 * 4];
                int len;
                while((len=in.read(buf))>0) {
                    out.write(buf, 0, len);
                }
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
    public void endGraph() {
    }

}