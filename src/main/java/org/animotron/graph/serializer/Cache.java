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

import org.animotron.utils.MessageDigester;

import java.io.*;

import static org.animotron.graph.AnimoGraph.getStorage;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public abstract class Cache {

    private final File CACHE_STORAGE = new File(getStorage(), "cache");

    {
		CACHE_STORAGE.mkdirs();
	}

	public File getSorage() {
        return new File(CACHE_STORAGE, name());
    }

    protected abstract String name();

    private File dir(String hash) throws FileNotFoundException {
        return new File(new File(getSorage(), hash.substring(0, 2)), hash.substring(0, 4));
    }

    protected final OutputStream out(byte[] hash) throws FileNotFoundException {
        String hex = MessageDigester.byteArrayToHex(hash);
        File dir = dir(hex);
        dir.mkdirs();
        return new FileOutputStream(new File(dir, hex));
    }

    protected final InputStream in(byte[] hash) throws FileNotFoundException {
        String hex = MessageDigester.byteArrayToHex(hash);
        return new FileInputStream(new File(dir(hex), hex));
    }

//    protected final void cache(Input)

}