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
import org.animotron.graph.handler.StAXGraphHandler;
import org.animotron.graph.traverser.MLResultTraverser;
import org.neo4j.graphdb.Relationship;

import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;
import java.io.IOException;
import java.io.OutputStream;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class XMLResultSerializer {
	
    public static final WstxOutputFactory OUTPUT_FACTORY = new WstxOutputFactory();

    static {
        WriterConfig conf = OUTPUT_FACTORY.getConfig();
        conf.doSupportNamespaces(true);
        conf.enableAutomaticNamespaces(true);
    }

    private static XMLStreamWriter getXMLStreamWriter(OutputStream out) throws IOException {
        try {
            return OUTPUT_FACTORY.createXMLStreamWriter(out);
        } catch (XMLStreamException e) {
            throw new IOException(e);
        }
    }
    public static void serialize(Relationship r, OutputStream out) throws IOException {
        serialize(r, r, out);
    }

    public static void serialize(Relationship start_op, Relationship r, OutputStream out) throws IOException {
        XMLStreamWriter writer = getXMLStreamWriter(out);
		MLResultTraverser._.traverse(new StAXGraphHandler(writer), start_op, r);
    }
}