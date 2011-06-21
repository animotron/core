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
package org.animotron.graph;

import java.io.OutputStream;

import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;

import org.animotron.graph.stax.StAXGraphSerializer;
import org.neo4j.graphdb.Relationship;

import com.ctc.wstx.api.WriterConfig;
import com.ctc.wstx.stax.WstxOutputFactory;

/**
 * 
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public abstract class GraphSerializer {
	
	public static final WstxOutputFactory OUTPUT_FACTORY = new WstxOutputFactory();
	static {
		WriterConfig conf = OUTPUT_FACTORY.getConfig();
		conf.doSupportNamespaces(true);
		conf.enableAutomaticNamespaces(true);
	}

	public static void serialize(Relationship r, OutputStream out) throws XMLStreamException {
		
        XMLStreamWriter writer = OUTPUT_FACTORY.createXMLStreamWriter(out);
        
        GraphTraverse serializer = new GraphTraverse(new StAXGraphSerializer(writer));
        serializer.traverse(r);
	}
	
}