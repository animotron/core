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
import org.animotron.graph.builder.StreamGraphBuilder;
import org.animotron.statement.Statement;
import org.animotron.statement.ml.*;
import org.animotron.statement.value.AbstractValue;
import org.animotron.statement.value.VALUE;

import javax.xml.namespace.QName;
import javax.xml.stream.XMLStreamConstants;
import javax.xml.stream.XMLStreamReader;
import java.io.IOException;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 * 
 */
public class StAXExpression extends AbstractExpression {
	
	private XMLStreamReader reader;
	
    public StAXExpression(XMLStreamReader reader) throws Exception {
        this(new StreamGraphBuilder(), reader);
    }

    public StAXExpression(GraphBuilder builder, XMLStreamReader reader) throws Exception {
        super(builder);
        this.reader = reader;
        builder.build(this);
    }

    @Override
    public void build() throws Exception {
        while (reader.hasNext()) {
            switch (reader.getEventType()) {
                case XMLStreamConstants.START_ELEMENT :
                    startElement();
                    break;
                case XMLStreamConstants.END_ELEMENT :
                    builder.end();
                    break;
                case XMLStreamConstants.PROCESSING_INSTRUCTION :
                    String target = reader.getPITarget();
                    String data = reader.getPIData();
                    build(PI._, (target.isEmpty()) ? value(data) : _(name(target), value(data)));
                    break;
                case XMLStreamConstants.DTD :
                    String dtd = reader.getText();
                    build(DTD._, dtd.isEmpty() ? null : dtd);
                    break;
                case XMLStreamConstants.ENTITY_REFERENCE :
                    build(ENTITY._, _(name(reader.getText())));
                    break;
                case XMLStreamConstants.CDATA :
                    String cdata = reader.getText();
                    build(CDATA._, cdata.isEmpty() ? null : cdata);
                    break;
                case XMLStreamConstants.COMMENT :
                    String comment = reader.getText();
                    build(COMMENT._, comment.isEmpty() ? null : comment);
                    break;
                case XMLStreamConstants.CHARACTERS :
                    String text = AbstractValue.removeWS(reader.getText());
                    if (!text.isEmpty())
                        build(VALUE._, AbstractValue.value(text));
            }
            reader.next();
        }
    }

    private void build(Statement s, Object reference) throws AnimoException, IOException {
        builder._(s, reference);
    }


    private void startElement() throws AnimoException, IOException {
        builder.start(ELEMENT._, _(name(qname(reader.getName()))));
        for (int i = 0; i < reader.getNamespaceCount(); i++) {
            String namespace = reader.getNamespaceURI(i);
            String prefix = reader.getNamespacePrefix(i);
            build(NS._, prefix.isEmpty() ? _(value(namespace)) : _(name(prefix), value(namespace)));
        }
        for (int i = 0; i < reader.getAttributeCount(); i++) {
            build(ATTRIBUTE._, _(name(qname(reader.getAttributeName(i))), value(AbstractValue.value(reader.getAttributeValue(i)))));
        }
    }

    private String qname(QName qname) {
        if (qname.getPrefix().isEmpty())
            return qname.getLocalPart();
        StringBuilder s = new StringBuilder();
        s.append(qname.getPrefix()); s.append(":"); s.append(qname.getLocalPart());
        return s.toString();
    }

    private Object[][] _(Object[]... o) {
        return o;
    }

    private Object[] _(Statement s, Object o) {
        Object[] a = {s, o};
        return a;
    }

    private Object[] name(String name) {
        return _(NAME._, name);
    }

    private Object[] value(Object value) {
        return _(VALUE._, value);
    }

}
