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
package org.animotron.graph.builder;

import org.animotron.exception.AnimoException;
import org.animotron.statement.ml.*;
import org.neo4j.graphdb.Relationship;

import javax.xml.namespace.QName;
import javax.xml.stream.XMLStreamConstants;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 * 
 */
public class StAXBuilder extends GraphBuilder {
	
	private XMLStreamReader reader;
	
	public StAXBuilder(XMLStreamReader reader) throws XMLStreamException {
		this.reader = reader;
	}

	public Relationship build() throws XMLStreamException, AnimoException {
		
		startGraph();
		
        while (reader.hasNext()) {

            switch (reader.getEventType()) {

                case XMLStreamConstants.START_ELEMENT :
                    startElement();
                    break;

                case XMLStreamConstants.END_ELEMENT :
                    end();
                    break;

                case XMLStreamConstants.PROCESSING_INSTRUCTION :
                    pi();
                    break;

                case XMLStreamConstants.DTD :
                    dtd();
                    break;

                case XMLStreamConstants.ENTITY_REFERENCE :
                    entity();
                    break;

                case XMLStreamConstants.CDATA :
                    cdata();
                    break;

                case XMLStreamConstants.COMMENT :
                    comment();
                    break;

                case XMLStreamConstants.CHARACTERS :
                    text();

            }

            reader.next();
        }

        endGraph();

        return getRelationship();
			
	}

    private void pi() {
        start(PI._);
            String target = reader.getPITarget();
            String data = reader.getPIData();
            if (!target.isEmpty()) {
                start(NAME._, target);
                end();
            }
            start(TEXT._, data);
            end();
        end();
    }

    private void dtd() {
        start(DTD._);
            String text = reader.getText();
            if (!text.isEmpty()) {
                start(text);
                end();
            }
        end();
    }

    private void entity() {
        start(ENTITY._);
            start(NAME._, reader.getText());
            end();
        end();
    }

    private void text() {
		String text = reader.getText();
        if (!text.isEmpty()) {
			start(text);
			end();
		}
	}

	private void comment() {
		start(COMMENT._);
            String text = reader.getText();
            if (!text.isEmpty()) {
                start(text);
                end();
            }
		end();
	}

	private void cdata() {
		start(CDATA._);
            String text = reader.getText();
            if (!text.isEmpty()) {
                start(text);
                end();
            }
		end();
	}

	private void startElement() {
        start(ELEMENT._);
            start(NAME._, qname(reader.getName()));
            end();
            for (int i = 0; i < reader.getNamespaceCount(); i++) {
                start(NS._);
                    String namespace = reader.getNamespaceURI(i);
                    String prefix = reader.getNamespacePrefix(i);
                    if (prefix != null) {
                        start(NAME._, prefix);
                        end();
                    }
                    start(namespace);
                    end();
                end();
            }
            for (int i = 0; i < reader.getAttributeCount(); i++) {
                start(ATTRIBUTE._);
                    start(NAME._, qname(reader.getAttributeName(i)));
                    end();
                    start(reader.getAttributeValue(i));
                    end();
                end();
            }
	}

    private String qname(QName qname) {

        if (qname.getPrefix().isEmpty())
            return qname.getLocalPart();

        StringBuilder s = new StringBuilder();
        s.append(qname.getPrefix()); s.append(":"); s.append(qname.getLocalPart());
        return s.toString();

    }
	
}
