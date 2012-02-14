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

import org.animotron.graph.handler.BinaryGraphHandler;
import org.animotron.graph.handler.GraphHandler;
import org.animotron.graph.traverser.ResultTraverser;

import java.io.IOException;
import java.io.OutputStream;
import java.io.Writer;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class BinarySerializer extends AbstractSerializer {
	
    public static BinarySerializer _ = new BinarySerializer();
    private BinarySerializer() {super(ResultTraverser._);}

    @Override
    protected GraphHandler handler(OutputStream out) throws IOException {
        return new BinaryGraphHandler(out);
    }

    @Override
    protected GraphHandler handler(Writer out) throws IOException {
        throw new UnsupportedOperationException();
    }

    @Override
    protected GraphHandler handler(StringBuilder out) {
        throw new UnsupportedOperationException();
    }

}