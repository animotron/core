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
package org.animotron.graph.handler;

import org.animotron.statement.Statement;
import org.animotron.statement.ml.*;
import org.neo4j.graphdb.Relationship;

import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;
import java.io.IOException;

/**
 * @author <a href="mailto:gazdovskyd@gmail.com">Evgeny Gazdovsky</a>
 * 
 */
public class StAXGraphHandler extends AbstractGraphHandler {
	
	private XMLStreamWriter writer;
	
	public StAXGraphHandler(XMLStreamWriter writer) {
		this.writer = writer;
	}
	
	@Override
	public void start(Statement statement, Relationship r) throws IOException {
		try {
			if (statement instanceof ATTRIBUTE) {
				String prefix = null;//statement.prefix(r);
				String ns = null;//statement.namespace(r);
				String name = statement.name(r);
				String value = statement.value(r);
				if (prefix == null && ns == null) {
					writer.writeAttribute(name, value);
				} else if (prefix == null) {
					writer.writeAttribute(ns, name, value);
				} else {
					writer.writeAttribute(prefix, ns, name, value);
				}
				
			} else if (statement instanceof TEXT) {
				writer.writeCharacters(statement.value(r));
				
			} else if (statement instanceof COMMENT){
				writer.writeComment(statement.value(r));
				
			} else if (statement instanceof CDATA){
				writer.writeCData(statement.value(r));
				
			} else if (statement instanceof ELEMENT) {
				String prefix = null;//statement.prefix(r);
				String ns = null;//statement.namespace(r);
				String name = statement.name(r);
				if (prefix == null && ns == null) {
					writer.writeStartElement(name);
				} else if (prefix == null) {
					writer.writeStartElement(ns, name);
				} else {
					writer.writeStartElement(prefix, name, ns);
				}
				
			}
			
		} catch (XMLStreamException e) {
            throw new IOException(e);
		}
	}

	@Override
	public void end(Statement statement, Relationship r) throws IOException {
		try {
			if (statement instanceof ELEMENT) {
				writer.writeEndElement();
			}
		} catch (XMLStreamException e) {
            throw new IOException(e);
		}
	}

	@Override
	public void startGraph() throws IOException {
		try {
			writer.writeStartDocument();
		} catch (XMLStreamException e) {
            throw new IOException(e);
		}
	}

	@Override
	public void endGraph() throws IOException {
		try {
			writer.writeEndDocument();
		} catch (XMLStreamException e) {
            throw new IOException(e);
		}
	}

}