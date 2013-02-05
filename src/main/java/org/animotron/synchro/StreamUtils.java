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

import java.io.*;
import java.nio.ByteBuffer;

/**
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class StreamUtils  {

    public final static byte START_GRAPH = 1;
    public final static byte START_RELATIONSHIP = 2;
    public final static byte REF = 3;
    public final static byte END_RELATIONSHIP = 0;
    public final static byte FS = 4;
    public final static byte END_GRAPH = -1;

    public final static byte STRING = 0;
    public final static byte LONG = 1;
    public final static byte DOUBLE = 2;
    public final static byte BOOLEAN = 3;

    public static byte[] readBytes(InputStream is) throws IOException {
        byte[] size  =  new byte[4];
        is.read(size);
        byte[] b = new byte[ByteBuffer.wrap(size).getInt()];
        is.read(b);
        return b;
    }

    public static void writeBytes(OutputStream os, byte[] b) throws IOException {
        byte[] size = new byte[4];
        ByteBuffer.wrap(size).putInt(b.length);
        os.write(size);
        os.write(b);
    }

    public static String readString(InputStream is) throws IOException {
        return new String(readBytes(is));
    }

    public static void writeString(OutputStream os, String d) throws IOException {
        os.write(STRING);
        writeBytes(os, d.getBytes());
    }

    public static long readLong(InputStream is) throws IOException {
        byte[] b = new byte[8];
        is.read(b);
        return ByteBuffer.wrap(b).getLong();
    }

    public static void writeLong(OutputStream os, Long d) throws IOException {
        os.write(LONG);
        byte[] b = new byte[8];
        ByteBuffer.wrap(b).putLong(d);
        os.write(b);
    }

    public static double readDouble(InputStream is) throws IOException {
        byte[] b = new byte[8];
        is.read(b);
        return ByteBuffer.wrap(b).getDouble();
    }

    public static void writeDouble(OutputStream os, Double d) throws IOException {
        os.write(DOUBLE);
        byte[] b = new byte[8];
        ByteBuffer.wrap(b).putDouble(d);
        os.write(b);
    }

    public static boolean readBoolean(InputStream is) throws IOException {
        int b =  is.read();
        return b == 1;
    }

    public static void writeBoolean(OutputStream os, Boolean d) throws IOException {
        os.write(BOOLEAN);
        byte b;
        if (d) {
            b = 1;
        } else {
            b = 0;
        }
        os.write(b);
    }

}
