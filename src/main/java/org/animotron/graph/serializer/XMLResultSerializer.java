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

import com.ctc.wstx.api.WriterConfig;
import com.ctc.wstx.stax.WstxOutputFactory;
import org.animotron.graph.handler.GraphHandler;
import org.animotron.graph.handler.StAXGraphHandler;
import org.animotron.graph.traverser.MLResultTraverser;

import javax.xml.stream.XMLStreamException;
import java.io.IOException;
import java.io.OutputStream;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class XMLResultSerializer extends CachedSerializer {
	
    public final static WstxOutputFactory OUTPUT_FACTORY = new WstxOutputFactory();

    public static XMLResultSerializer _ = new XMLResultSerializer();
    private XMLResultSerializer() {
        super(MLResultTraverser._, ".xml");
        WriterConfig conf = OUTPUT_FACTORY.getConfig();
        conf.doSupportNamespaces(true);
        conf.enableAutomaticNamespaces(false);
    }

    @Override
    protected GraphHandler handler(OutputStream out) throws IOException {
        try {
            return new StAXGraphHandler(OUTPUT_FACTORY.createXMLStreamWriter(out));
        } catch (XMLStreamException e) {
            throw new IOException(e);
        }
    }

    @Override
    protected GraphHandler handler(StringBuilder out) {
        throw new UnsupportedOperationException();
    }

}