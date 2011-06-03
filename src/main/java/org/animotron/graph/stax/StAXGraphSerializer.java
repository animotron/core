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
package org.animotron.graph.stax;

import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;

import org.animotron.Statement;
import org.animotron.graph.AbstractGraphSerializer;
import org.animotron.instruction.ml.ATTRIBUTE;
import org.animotron.instruction.ml.CDATA;
import org.animotron.instruction.ml.COMMENT;
import org.animotron.instruction.ml.TEXT;
import org.animotron.instruction.ml.ValueInstruction;
import org.neo4j.graphdb.Relationship;

/**
 * @author <a href="mailto:gazdovskyd@gmail.com">Evgeny Gazdovsky</a>
 * 
 */
public class StAXGraphSerializer extends AbstractGraphSerializer {
	
	private XMLStreamWriter writer;
	
	public StAXGraphSerializer(XMLStreamWriter writer) {
		this.writer = writer;
	}

	@Override
	public void start(Statement statement, Relationship r) {
		try {
			if (statement instanceof ATTRIBUTE) {
				writer.writeAttribute(statement.prefix(r), statement.namespace(r), statement.name(r), statement.value(r));
				
			} else if (statement instanceof TEXT) {
				writer.writeCharacters(statement.value(r));
				
			} else if (statement instanceof COMMENT){
				writer.writeComment(statement.value(r));
				
			} else if (statement instanceof CDATA){
				writer.writeCData(statement.value(r));
				
			} else {
				writer.writeStartElement(statement.prefix(r), statement.namespace(r), statement.name(r));				
			}
			
		} catch (XMLStreamException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	@Override
	public void end(Statement statement, Relationship r) {
		try {
			if (statement instanceof ValueInstruction) {
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

	@Override
	public void endDocument() {
		try {
			writer.writeEndDocument();
		} catch (XMLStreamException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	
}
