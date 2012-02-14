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
package org.animotron.graph.serializer;

import org.animotron.graph.handler.GraphHandler;
import org.animotron.graph.traverser.AnimoTraverser;
import org.animotron.manipulator.QCAVector;
import org.neo4j.graphdb.Relationship;

import java.io.IOException;
import java.io.OutputStream;
import java.io.Writer;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public abstract class AbstractSerializer {

    protected final AnimoTraverser traverser;

    protected AbstractSerializer(AnimoTraverser traverser) {
        this.traverser = traverser;
    }

    protected abstract GraphHandler handler(OutputStream out) throws IOException;

    protected abstract GraphHandler handler(Writer out) throws IOException;

    protected abstract GraphHandler handler(StringBuilder out);

    public final void serialize(Relationship r, OutputStream out) throws IOException {
        traverser.traverse(handler(out), r);
    }

    public final void serialize(QCAVector v, OutputStream out) throws IOException {
        traverser.traverse(handler(out), v);
    }

    public final void serialize(Relationship r, StringBuilder out) throws IOException {
        traverser.traverse(handler(out), r);
    }

    public final void serialize(QCAVector v, StringBuilder out) throws IOException {
        traverser.traverse(handler(out), v);
    }

    public final void serialize(Relationship r, Writer out) throws IOException {
        traverser.traverse(handler(out), r);
    }

    public final void serialize(QCAVector v, Writer out) throws IOException {
        traverser.traverse(handler(out), v);
    }

    public String serialize(Relationship r) throws IOException {
        StringBuilder out = new StringBuilder(256);
        traverser.traverse(handler(out), r);
        return out.toString();
    }

    public String serialize(QCAVector v) throws IOException {
        StringBuilder out = new StringBuilder(256);
        traverser.traverse(handler(out), v);
        return out.toString();
    }

}