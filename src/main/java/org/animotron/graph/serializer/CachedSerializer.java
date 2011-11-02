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

import org.animotron.cache.Cache;
import org.animotron.graph.traverser.AnimoTraverser;
import org.animotron.manipulator.PFlow;
import org.neo4j.graphdb.Relationship;

import java.io.IOException;
import java.io.OutputStream;

import static org.animotron.utils.MessageDigester.byteArrayToHex;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public abstract class CachedSerializer extends AbstractSerializer {

    private String ext;

    protected CachedSerializer(AnimoTraverser traverser, String ext){
        super(traverser);
        this.ext = ext;
	}

    private String key (Relationship r) throws IOException {
        StringBuilder s = new StringBuilder(2);
        s.append(byteArrayToHex(DigestSerializer._.serialize(r)));
        s.append(ext);
        return s.toString();
    }

    private String key (PFlow pf, Relationship r) throws IOException {
        StringBuilder s = new StringBuilder(4);
        s.append(byteArrayToHex(DigestSerializer._.serialize(r)));
        s.append("-");
        s.append(byteArrayToHex(pf.getPathHash()));
        s.append(ext);
        return s.toString();
    }

    public final void serialize(Relationship r, OutputStream out, Cache cache) throws IOException {
        String key = key(r);
        if (cache.available(key)) {
            cache.get(key, out);
        } else {
            OutputStream os = cache.stream(key, out);
            serialize(r, os);
            os.close();
        }
    }

    public final void serialize(PFlow pf, Relationship r, OutputStream out, Cache cache) throws IOException {
        String key = key(pf, r);
        if (cache.available(key)) {
            cache.get(key, out);
        } else {
            OutputStream os = cache.stream(key, out);
            serialize(pf, r, os);
            os.close();
        }
    }

    public final void serialize(Relationship r, StringBuilder out, Cache cache) throws IOException {
        String key = key(r);
        if (cache.available(key)) {
            cache.get(key, out);
        } else {
            OutputStream os = cache.stream(key, out);
            serialize(r, os);
            os.close();
        }
    }

    public final void serialize(PFlow pf, Relationship r, StringBuilder out, Cache cache) throws IOException {
        String key = key(pf, r);
        if (cache.available(key)) {
            cache.get(key, out);
        } else {
            OutputStream os = cache.stream(key, out);
            serialize(pf, r, os);
            os.close();
        }
    }

    public final String serialize(Relationship r, Cache cache) throws IOException {
        String key = key(r);
        if (cache.available(key)) {
            return cache.get(key);
        } else {
            StringBuilder out = new StringBuilder(1024);
            OutputStream os = cache.stream(key, out);
            serialize(r, os);
            os.close();
            return out.toString();
        }
    }

	public final String serialize(PFlow pf, Relationship r, Cache cache) throws IOException {
        String key = key(pf, r);
        if (cache.available(key)) {
            return cache.get(key);
        } else {
            StringBuilder out = new StringBuilder(1024);
            OutputStream os = cache.stream(key, out);
            serialize(pf, r, os);
            os.close();
            return out.toString();
        }
    }



}