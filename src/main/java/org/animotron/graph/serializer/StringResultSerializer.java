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

import org.animotron.graph.handler.TextGraphHandler;
import org.animotron.graph.traverser.GraphAnimoResultTraverser;
import org.neo4j.graphdb.Relationship;

import java.io.OutputStream;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class StringResultSerializer {
	
    public static void serialize(Relationship r, OutputStream out) throws InterruptedException {
        serialize(r, r, out);
    }

    public static void serialize(Relationship start_op, Relationship r, OutputStream out) throws InterruptedException {
        GraphAnimoResultTraverser._.traverse(new TextGraphHandler(out), start_op, r);
    }

    public static String serialize(Relationship r) throws InterruptedException {
        return serialize(r, r);
    }

    public static String serialize(Relationship start_op, Relationship r) throws InterruptedException {
        StringBuilder out = new StringBuilder(1024);
        GraphAnimoResultTraverser._.traverse(new TextGraphHandler(out), start_op, r);
        return out.toString();
    }

}