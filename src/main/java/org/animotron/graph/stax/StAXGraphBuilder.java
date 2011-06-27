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

import javax.xml.stream.XMLStreamConstants;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;

import org.animotron.exception.EBuilderTerminated;
import org.animotron.graph.GraphBuilder;
import org.animotron.instruction.ml.ATTRIBUTE;
import org.animotron.instruction.ml.CDATA;
import org.animotron.instruction.ml.COMMENT;
import org.animotron.instruction.ml.TEXT;
import org.neo4j.graphdb.Relationship;



/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 * 
 */
public class StAXGraphBuilder extends GraphBuilder {
	
	private XMLStreamReader reader;
	
	public StAXGraphBuilder(XMLStreamReader reader) {
		this.reader = reader;
	}

	public Relationship build() throws XMLStreamException {
		
		startGraph();
		
		while (reader.hasNext()) {
			
			switch (reader.getEventType()) {
			
			case XMLStreamConstants.START_ELEMENT : 
				startElement(reader);
				break;
			
			case XMLStreamConstants.END_ELEMENT : 
				end();
				break;
			
			case XMLStreamConstants.CDATA : 
				cdata(reader);
				break;
			
			case XMLStreamConstants.COMMENT : 
				comment(reader);
				break;
			
			case XMLStreamConstants.CHARACTERS : 
				text(reader);
			
			}
			
			reader.next();
		}
		
		try {
			endGraph();
		} catch (EBuilderTerminated e) {
			throw new XMLStreamException(e);
		}
		
		return getRelationship();
		
	}

	private void text(XMLStreamReader reader) {
		String value = removeWS(reader.getText());
		if (value != null) {
			start(TEXT._, null, null, null, value);
			end();
		}
	}

	private void comment(XMLStreamReader reader) {
		start(COMMENT._, null, null, null, reader.getText());
		end();
	}

	private void cdata(XMLStreamReader reader) {
		start(CDATA._, null, null, null, reader.getText());
		end();
	}

	private void startElement(XMLStreamReader reader) {
		start(reader.getPrefix(), reader.getNamespaceURI(), reader.getLocalName(), null);
		for (int i = 0; i < reader.getAttributeCount(); i++) {
			start(ATTRIBUTE._,
					reader.getAttributePrefix(i),
					reader.getAttributeNamespace(i), 
					reader.getAttributeLocalName(i), 
					reader.getAttributeValue(i));
			end();
		}
	}
	
}
