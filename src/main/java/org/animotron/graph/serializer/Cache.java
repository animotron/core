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
package org.animotron.graph.serializer;

import org.animotron.graph.traverser.AnimoTraverser;
import org.animotron.utils.MessageDigester;
import org.neo4j.graphdb.Relationship;

import java.io.*;

import static org.animotron.graph.AnimoGraph.getStorage;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public abstract class Cache extends AbstractSerializer {

    private static final File CACHE_STORAGE = new File(getStorage(), "cache");
    private File storage;

    static {
        CACHE_STORAGE.mkdirs();
    }

    protected Cache (String name, AnimoTraverser traverser){
        super(traverser);
        storage = new File(CACHE_STORAGE, name);
	}

    private File dir(String hash) throws FileNotFoundException {
        return new File(new File(storage, hash.substring(0, 2)), hash.substring(0, 4));
    }

    protected final File file(String hash) throws FileNotFoundException {
        return new File(dir(hash), hash);
    }

    protected final OutputStream out(String hash) throws FileNotFoundException {
        File dir = dir(hash);
        dir.mkdirs();
        return new FileOutputStream(new File(dir, hash));
    }

    public final void cache(Relationship r, OutputStream out) throws IOException {
        String hash = MessageDigester.byteArrayToHex(DigestSerializer._.serialize(r));
        File file = file(hash);
        if (file.exists()) {
            out(new FileInputStream(file), out);
        } else {
            OutputStream cache = new CacheStream(hash, out);
            serialize(r, cache);
            cache.close();
        }
    }

    public final void cache(Relationship r, StringBuilder out) throws IOException {
        String hash = MessageDigester.byteArrayToHex(DigestSerializer._.serialize(r));
        File file = file(hash);
        if (file.exists()) {
            out(new FileInputStream(file), out);
        } else {
            OutputStream cache = new CacheBuilder(hash, out);
            serialize(r, cache);
            cache.close();
        }
    }

    public final String cache(Relationship r) throws IOException {
        String hash = MessageDigester.byteArrayToHex(DigestSerializer._.serialize(r));
        File file = file(hash);
        if (file.exists()) {
            return out(new FileInputStream(file));
        } else {
            StringBuilder out = new StringBuilder(1024);
            OutputStream cache = new CacheBuilder(hash, out);
            serialize(r, cache);
            cache.close();
            return out.toString();
        }
    }

    private void out(InputStream in, OutputStream out) throws IOException {
        int len;
        byte buf[] = new byte[1024 * 4];
        while((len=in.read(buf))>0) {
            out.write(buf,0,len);
        }
        in.close();
    }

    private void out(InputStream in, StringBuilder out) throws IOException {
        int len;
        char[] buf= new char[1024 * 4];
        BufferedReader reader = new BufferedReader(new InputStreamReader(in, "UTF-8"));
        while((len=reader.read(buf))>0) {
            out.append(buf, 0, len);
        }
        in.close();
    }

    private String out(InputStream in) throws IOException {
        StringBuilder out = new StringBuilder(1024);
        out(in, out);
        return out.toString();
    }

    private class CacheStream extends OutputStream {

        private String hash;
        private OutputStream out;

        public CacheStream(String hash, OutputStream out) {
            this.hash = hash;
            this.out = out;
        }

        @Override
        public void write(int b) throws IOException {
        }

    }

    private class CacheBuilder extends OutputStream {

        private String hash;
        private StringBuilder out;

        public CacheBuilder(String hash, StringBuilder out) {
            this.hash = hash;
            this.out = out;
        }

        @Override
        public void write(int b) throws IOException {
        }

    }

}