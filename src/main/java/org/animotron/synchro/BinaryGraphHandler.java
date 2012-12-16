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
package org.animotron.synchro;

import org.animotron.graph.Properties;
import org.animotron.graph.handler.GraphHandler;
import org.animotron.graph.serializer.DigestSerializer;
import org.animotron.manipulator.Controller;
import org.animotron.statement.Statement;
import org.neo4j.graphdb.Relationship;

import java.io.*;
import java.nio.ByteBuffer;

import static org.animotron.synchro.StreamUtils.*;

/**
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class BinaryGraphHandler implements GraphHandler {

    private OutputStream os;
    private String fs = null;

    public BinaryGraphHandler(OutputStream os) {
        this.os = os;
    }

    @Override
    public void start(Statement statement, Statement parent, Relationship r, int level, boolean isOne, int pos, boolean isLast) throws IOException {
        if (level == 0) {
            if (Properties.FS.has(r)) {
                fs = (String) Properties.FS.get(r);
            }
            byte[] hash = DigestSerializer.serialize(r);
            writeBytes(null, hash);
        }
        os.write(START_RELATIONSHIP);
        writeBytes(null, statement.name().getBytes());
        if (r != null) {
            Object o = statement.reference(r);
            if (o != null) {
                os.write(REF);
                if (o instanceof String) {
                    writeString(os, (String) o);
                } else if (o instanceof Long) {
                    writeLong(os, (Long) o);
                } else if (o instanceof Double) {
                    writeDouble(os, (Double) o);
                } else if (o instanceof Boolean) {
                    writeBoolean(os, (Boolean) o);
                }
            }
        }
    }

    @Override
    public void end(Statement statement, Statement parent, Relationship r, int level, boolean isOne, int pos, boolean isLast) throws IOException {
        os.write(END_RELATIONSHIP);
    }

    @Override
    public void startGraph() throws IOException {
        os.write(START_GRAPH);
    }

    @Override
    public void endGraph() throws IOException {
        if (fs != null) {
            os.write(FS);
            writeBytes(null, fs.getBytes());
            File file = new File(fs);
            byte[] size = new byte[8];
            ByteBuffer.wrap(size).putLong(file.length());
            os.write(size);
            InputStream is = new FileInputStream(file);
            byte buf[] = new byte[1024 * 4];
            int len;
            while((len=is.read(buf))>0) {
                os.write(buf, 0, len);
            }
            is.close();
        }
        os.write(END_GRAPH);
    }

    @Override
    public Controller getController() {
        return null;
    }
}
