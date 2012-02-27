/*
 *  Copyright (C) 2011-2012 The Animo Project
 *  http://animotron.org
 *  	
 *  This file is part of Animotron.
 *  
 *  Animotron is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU Affero General Public License as 
 *  published by the Free Software Foundation, either version 3 of 
 *  the License, or (at your option) any later version.
 *  
 *  Animotron is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Affero General Public License for more details.
 *  
 *  You should have received a copy of 
 *  the GNU Affero General Public License along with Animotron.  
 *  If not, see <http://www.gnu.org/licenses/>.
 */
package org.animotron.expression;

import org.animotron.exception.AnimoException;
import org.animotron.graph.builder.GraphBuilder;
import org.animotron.statement.Statement;
import org.animotron.statement.ml.*;
import org.animotron.statement.operator.THE;
import org.animotron.statement.value.AbstractValue;
import org.animotron.statement.value.VALUE;

import javax.xml.stream.XMLStreamConstants;
import javax.xml.stream.XMLStreamReader;
import java.io.IOException;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 * 
 */
public class StAXExpression extends AbstractStAXExpression {

    public StAXExpression(XMLStreamReader reader) {
        super(reader);
    }

    public StAXExpression(XMLStreamReader reader, String name) {
        super(reader, name);
    }

    public StAXExpression(GraphBuilder builder, XMLStreamReader reader) {
        super(builder, reader);
    }

    public StAXExpression(GraphBuilder builder, XMLStreamReader reader, String name) {
        super(builder, reader, name);
    }

    @Override
    public void build() throws Throwable {
        builder.start(THE._, name);
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
                    //builder.start(PI._, _(name(target), value(data)));
                    //builder.end();
                    //XXX: target can't be null/empty ?
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
        builder.end();
    }

    private void build(Statement s, Object reference) throws AnimoException, IOException {
        builder._(s, reference);
    }

    protected void startElement() throws AnimoException, IOException {
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

}
