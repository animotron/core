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

import org.animotron.graph.handler.GraphHandler;
import org.animotron.graph.traverser.AnimoTraverser;
import org.animotron.manipulator.PFlow;
import org.neo4j.graphdb.Relationship;

import java.io.IOException;
import java.io.OutputStream;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public abstract class AbstractSerializer {

    private AnimoTraverser traverser;

    protected AbstractSerializer(AnimoTraverser traverser) {
        this.traverser = traverser;
    }

    protected abstract GraphHandler handler(OutputStream out) throws IOException;

    protected abstract GraphHandler handler(StringBuilder out);

    public final void serialize(Relationship r, OutputStream out) throws IOException {
        traverser.traverse(handler(out), r);
    }

    public final void serialize(Relationship r, StringBuilder out) throws IOException {
        traverser.traverse(handler(out), r);
    }

    public final String serialize(Relationship r) throws IOException {
        StringBuilder out = new StringBuilder(1024);
        traverser.traverse(handler(out), r);
        return out.toString();
    }

    public final void serialize(PFlow pf, Relationship r, OutputStream out) throws IOException {
        traverser.traverse(handler(out), pf, r);
    }

    public final void serialize(PFlow pf, Relationship r, StringBuilder out) throws IOException {
        traverser.traverse(handler(out), pf, r);
    }

	public final String serialize(PFlow pf, Relationship r) throws IOException {
        StringBuilder out = new StringBuilder(1024);
        traverser.traverse(handler(out), pf, r);
        return out.toString();
    }

}