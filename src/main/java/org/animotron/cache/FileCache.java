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
package org.animotron.cache;

import java.io.*;

import static org.animotron.graph.AnimoGraph.getStorage;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class FileCache implements Cache {

    public static FileCache _ = new FileCache();

    private FileCache() {
        CACHE_STORAGE.mkdirs();
    }

    private static final File CACHE_STORAGE = new File(getStorage(), "stream");

    private File dir(String hash) throws FileNotFoundException {
        return new File(new File(CACHE_STORAGE, hash.substring(0, 2)), hash.substring(0, 4));
    }

    protected final File file(String hash) throws FileNotFoundException {
        return new File(dir(hash), hash);
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

    @Override
    public boolean available(String key) throws IOException {
        return file(key).exists();
    }

    @Override
    public void get(String key, OutputStream out) throws IOException {
        int len;
        InputStream in = new FileInputStream(file(key));
        byte buf[] = new byte[1024 * 4];
        while((len=in.read(buf))>0) {
            out.write(buf,0,len);
        }
        in.close();
    }

    @Override
    public void get(String key, StringBuilder out) throws IOException {
        int len;
        InputStream in = new FileInputStream(file(key));
        char[] buf= new char[1024 * 4];
        BufferedReader reader = new BufferedReader(new InputStreamReader(in, "UTF-8"));
        while((len=reader.read(buf))>0) {
            out.append(buf, 0, len);
        }
        in.close();
    }

    @Override
    public String get(String key) throws IOException {
        StringBuilder out = new StringBuilder(1024);
        get(key, out);
        return out.toString();
    }

    @Override
    public OutputStream stream(String key, OutputStream out) throws IOException {
        return new CacheStream(key, out);
    }

    @Override
    public OutputStream stream(String key, StringBuilder out) throws IOException {
        return new CacheBuilder(key, out);
    }

    @Override
    public void drop(String key) throws IOException {
        File file = file(key);
        if (file.exists()) file.delete();
    }

    private abstract class AbstractCache extends OutputStream {

        protected OutputStream cache;

        public AbstractCache(String hash) throws FileNotFoundException {
            File dir = dir(hash);
            dir.mkdirs();
            cache = new FileOutputStream(new File(dir, hash));
        }

        @Override
        public void close() throws IOException {
            cache.close();
        }

    }

    private class CacheStream extends AbstractCache {

        private OutputStream out;

        public CacheStream(String hash, OutputStream out) throws FileNotFoundException {
            super(hash);
            this.out = out;
        }

        @Override
        public void write(int b) throws IOException {
            out.write(b);
            cache.write(b);
        }

        @Override
        public void write(byte b[], int off, int len) throws IOException {
            out.write(b, off, len);
            cache.write(b, off, len);
        }

    }

    private class CacheBuilder extends AbstractCache {

        private StringBuilder out;

        public CacheBuilder(String hash, StringBuilder out) throws FileNotFoundException {
            super(hash);
            this.out = out;
        }

        @Override
        public void write(int b) throws IOException {
            out.append((char)b);
            cache.write(b);
        }

        @Override
        public void write(byte b[], int off, int len) throws IOException {
            out.append(new String(b, off, len));
            cache.write(b, off, len);
        }

    }

}