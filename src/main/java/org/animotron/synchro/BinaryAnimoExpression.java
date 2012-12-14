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

import org.animotron.expression.AbstractExpression;
import org.animotron.statement.Statement;
import org.animotron.statement.Statements;

import java.io.*;

import static org.animotron.synchro.StreamUtils.*;

/**
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class BinaryAnimoExpression extends AbstractExpression {

    private final BufferedInputStream is;

    private byte[] hash;

    public BinaryAnimoExpression(InputStream is) {
        this.is = new BufferedInputStream(is);
    }

    @Override
    public void build() throws Throwable {
        int b;
        Object o = null;
        Statement s = null;
        while ((b = is.read()) != END_GRAPH) {
            switch (b) {
                case START_GRAPH :
                    hash = readBytes(is);
                    break;
                case START_RELATIONSHIP :
                    if (s != null) {
                        builder.start(s, o);
                    }
                    s = Statements.name(readString(is));
                    o = null;
                    break;
                case REF :
                    int type = is.read();
                    switch (type) {
                        case STRING :
                            o = readString(is);
                            break;
                        case LONG :
                            o = readLong(is);
                            break;
                        case DOUBLE :
                            o = readDouble(is);
                            break;
                        case BOOLEAN :
                            o = readBoolean(is);
                            break;
                    }
                    break;
                case END_RELATIONSHIP :
                    builder.end();
                    break;
                case FS :
                    String fs = readString(is);
                    File file = new File(fs);
                    long size = readLong(is);
                    BufferedOutputStream os = new BufferedOutputStream(new FileOutputStream(file));
                    for (int i = 0; i < size; i++) {
                        os.write(is.read());
                    }
                    os.close();

            }
        }

    }
}
