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
package org.animotron.expression;

import org.animotron.exception.AnimoException;
import org.animotron.graph.builder.GraphBuilder;
import org.animotron.graph.builder.MLGraphBuilder;
import org.animotron.statement.ml.*;

import javax.xml.namespace.QName;
import javax.xml.stream.XMLStreamConstants;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 * 
 */
public class StAXExpression extends Expression {
	
	private XMLStreamReader reader;
	
    public StAXExpression(XMLStreamReader reader) throws Exception {
        this(new MLGraphBuilder(), reader);
    }

    public StAXExpression(GraphBuilder builder, XMLStreamReader reader) throws Exception {
        super(builder);
        this.reader = reader;
        builder.build(this);
    }

    @Override
    public void build() throws XMLStreamException, AnimoException {
        builder.startGraph();
        while (reader.hasNext()) {
            switch (reader.getEventType()) {
                case XMLStreamConstants.START_ELEMENT :
                    startElement();
                    break;
                case XMLStreamConstants.END_ELEMENT :
                    builder.end();
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
        builder.endGraph();
    }

    private void pi() throws AnimoException {
        builder.start(PI._);
            String target = reader.getPITarget();
            String data = reader.getPIData();
            if (!target.isEmpty()) {
                builder.start(NAME._, target);
                builder.end();
            }
            builder.start(data);
            builder.end();
        builder.end();
    }

    private void dtd() throws AnimoException {
        builder.start(DTD._);
            String text = reader.getText();
            if (!text.isEmpty()) {
                builder.start(text);
                builder.end();
            }
        builder.end();
    }

    private void entity() throws AnimoException {
        builder.start(ENTITY._);
            builder.start(NAME._, reader.getText());
            builder.end();
        builder.end();
    }

    private void text() throws AnimoException {
        String text = reader.getText();
        if (!text.isEmpty()) {
            builder.start(text);
            builder.end();
        }
    }

    private void comment() throws AnimoException {
        builder.start(COMMENT._);
            String text = reader.getText();
            if (!text.isEmpty()) {
                builder.start(text);
                builder.end();
            }
        builder.end();
    }

    private void cdata() throws AnimoException {
        builder.start(CDATA._);
            String text = reader.getText();
            if (!text.isEmpty()) {
                builder.start(text);
                builder.end();
            }
        builder.end();
    }

    private void startElement() throws AnimoException {
        builder.start(ELEMENT._);
            builder.start(NAME._, qname(reader.getName()));
            builder.end();
            for (int i = 0; i < reader.getNamespaceCount(); i++) {
                builder.start(NS._);
                    String namespace = reader.getNamespaceURI(i);
                    String prefix = reader.getNamespacePrefix(i);
                    if (prefix != null) {
                        builder.start(NAME._, prefix);
                        builder.end();
                    }
                    builder.start(namespace);
                    builder.end();
                builder.end();
            }
            for (int i = 0; i < reader.getAttributeCount(); i++) {
                builder.start(ATTRIBUTE._);
                    builder.start(NAME._, qname(reader.getAttributeName(i)));
                    builder.end();
                    builder.start(reader.getAttributeValue(i));
                    builder.end();
                builder.end();
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
