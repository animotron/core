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
import org.animotron.graph.handler.TextGraphHandler;
import org.animotron.graph.traverser.AnimoResultTraverser;

import java.io.OutputStream;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class StringResultSerializer extends Cache {
	
    public static StringResultSerializer _ = new StringResultSerializer();
    private StringResultSerializer() {super("text", AnimoResultTraverser._);}

    @Override
    protected GraphHandler handler(OutputStream out) {
        return new TextGraphHandler(out);
    }

    @Override
    protected GraphHandler handler(StringBuilder out) {
        return new TextGraphHandler(out);
    }

}