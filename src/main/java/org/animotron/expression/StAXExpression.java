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
import org.animotron.statement.ml.*;
import org.animotron.statement.operator.DEF;
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
        builder.start(DEF._, name);
        process();
        builder.end();
    }

    protected void process() throws Throwable {
        while (reader.hasNext()) {
            switch (reader.getEventType()) {
                case XMLStreamConstants.START_ELEMENT :
                    startElement();
                    break;
                case XMLStreamConstants.END_ELEMENT :
                    endElement();
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
    }

    private void text() throws AnimoException, IOException {
        String text = AbstractValue.removeWS(reader.getText());
        if (!text.isEmpty())
            builder._(VALUE._, AbstractValue.value(text));
    }

    private void comment() throws AnimoException, IOException {
        String comment = reader.getText();
        builder._(COMMENT._, comment.isEmpty() ? null : comment);
    }

    private void cdata() throws AnimoException, IOException {
        String cdata = reader.getText();
        builder._(CDATA._, cdata.isEmpty() ? null : cdata);
    }

    private void entity() throws AnimoException, IOException {
        builder._(ENTITY._, _(name(reader.getText())));
    }

    private void dtd() throws AnimoException, IOException {
        String dtd = reader.getText();
        builder._(DTD._, dtd.isEmpty() ? null : dtd);
    }

    private void pi() throws AnimoException, IOException {
        String target = reader.getPITarget();
        String data = reader.getPIData();
        //builder.start(PI._, _(name(target), value(data)));
        //builder.end();
        //XXX: target can't be null/empty ?
        builder._(PI._, (target.isEmpty()) ? value(data) : _(name(target), value(data)));
    }

    protected void startElement() throws AnimoException, IOException {
        builder.start(ELEMENT._, _(name(qname(reader.getName()))));
        for (int i = 0; i < reader.getNamespaceCount(); i++) {
            String namespace = reader.getNamespaceURI(i);
            String prefix = reader.getNamespacePrefix(i);
            builder._(NS._, prefix.isEmpty() ? _(value(namespace)) : _(name(prefix), value(namespace)));
        }
        for (int i = 0; i < reader.getAttributeCount(); i++) {
            builder._(ATTRIBUTE._, _(name(qname(reader.getAttributeName(i))), value(AbstractValue.value(reader.getAttributeValue(i)))));
        }
    }

    protected void endElement() throws AnimoException, IOException {
        builder.end();
    }

}
