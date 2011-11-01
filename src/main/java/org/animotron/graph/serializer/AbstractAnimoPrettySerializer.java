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

import org.animotron.graph.handler.AnimoPrettyGraphHandler;
import org.animotron.graph.handler.GraphHandler;
import org.animotron.graph.traverser.AnimoTraverser;

import java.io.OutputStream;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class AbstractAnimoPrettySerializer extends AbstractAnimoSerializer {

    private AnimoTraverser traverser;

    protected AbstractAnimoPrettySerializer(String name, AnimoTraverser traverser) {
        super(name, traverser);
    }

    @Override
    protected GraphHandler handler(OutputStream out) {
        return new AnimoPrettyGraphHandler(out);
    }

    @Override
    protected GraphHandler handler(StringBuilder out) {
        return new AnimoPrettyGraphHandler(out);
    }

}