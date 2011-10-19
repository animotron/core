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

import org.animotron.graph.handler.AnimoGraphHandler;
import org.animotron.graph.handler.AnimoPrettyGraphHandler;
import org.animotron.graph.handler.GraphHandler;
import org.animotron.graph.traverser.AnimoTraverser;
import org.neo4j.graphdb.Relationship;

import java.io.IOException;
import java.io.OutputStream;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class AnimoSerializer {

    public static void serialize(Relationship r, OutputStream out) throws IOException {
        serialize(r, out, false);
    }

    public static void serialize(Relationship r, OutputStream out, boolean pretty) throws IOException {
        AnimoTraverser._.traverse(handler(out, pretty), null, r);
    }

    public static void serialize(Relationship r, StringBuilder out) throws IOException {
        serialize(r, out, false);
    }

    public static void serialize(Relationship r, StringBuilder out, boolean pretty) throws IOException {
        AnimoTraverser._.traverse(handler(out, pretty), null, r);
    }

    public static String serialize(Relationship r) throws IOException {
        return serialize(r, false);
    }

    public static String serialize(Relationship r, boolean pretty) throws IOException {
        StringBuilder out = new StringBuilder(1024);
        AnimoTraverser._.traverse(handler(out, pretty), null, r);
        return out.toString();
    }

    public static GraphHandler handler(OutputStream out, boolean pretty) {
        return pretty ? new AnimoPrettyGraphHandler(out) : new AnimoGraphHandler(out);
    }

    public static GraphHandler handler(StringBuilder out, boolean pretty) {
        return pretty ? new AnimoPrettyGraphHandler(out) : new AnimoGraphHandler(out);
    }

}