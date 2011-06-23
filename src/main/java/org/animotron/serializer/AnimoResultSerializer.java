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
package org.animotron.serializer;

import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;

import org.animotron.Statement;
import org.animotron.instruction.ml.ATTRIBUTE;
import org.animotron.instruction.ml.CDATA;
import org.animotron.instruction.ml.COMMENT;
import org.animotron.instruction.ml.ELEMENT;
import org.animotron.instruction.ml.TEXT;
import org.animotron.instruction.ml.ValueInstruction;
import org.animotron.operator.IC;
import org.animotron.operator.relation.HAVE;
import org.codehaus.stax2.XMLStreamWriter2;
import org.neo4j.graphdb.Relationship;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class AnimoResultSerializer extends AbstractResultSerializer {
	
	private XMLStreamWriter writer;
	
	public AnimoResultSerializer(XMLStreamWriter writer) {
		this.writer = writer;
	}

	@Override
	public void start(Statement statement, Relationship r) {
		try {
			if (statement instanceof IC)
				statement = HAVE._;

			if (statement instanceof ATTRIBUTE) {
				String prefix = statement.prefix(r);
				String ns = statement.namespace(r);
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
				String prefix = statement.prefix(r);
				String ns = statement.namespace(r);
				String name = statement.name(r);
				if (prefix == null && ns == null) {
					writer.writeStartElement(name);
				} else if (prefix == null) {
					writer.writeStartElement(ns, name);
				} else {
					writer.writeStartElement(prefix, name, ns);
				}
				
			} else {
				writer.writeStartElement(statement.prefix(r), statement.name(r), statement.namespace(r));
				
			}
			
		} catch (XMLStreamException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	@Override
	public void end(Statement statement, Relationship r) {
		try {
			if (statement instanceof ValueInstruction ||
					statement instanceof ATTRIBUTE) {
				return;
			} else {
				writer.writeEndElement();
			}
		} catch (XMLStreamException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	@Override
	public void startDocument() {
		try {
			writer.writeStartDocument();
		} catch (XMLStreamException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	/* (non-Javadoc)
	 * @see org.animotron.graph.GraphHandler#endDocument()
	 */
//	@Override
	public void endDocument() {
		try {
			writer.writeEndDocument();
			writer.flush();
			if (writer instanceof XMLStreamWriter2) {
				((XMLStreamWriter2) writer).closeCompletely();
			} else {
				writer.close();
			}
		} catch (XMLStreamException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
}