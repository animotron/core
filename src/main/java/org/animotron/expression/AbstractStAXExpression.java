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

import org.animotron.graph.builder.GraphBuilder;
import org.animotron.graph.builder.StreamGraphBuilder;
import org.animotron.statement.Statement;
import org.animotron.statement.ml.QNAME;
import org.animotron.statement.value.VALUE;

import javax.xml.namespace.QName;
import javax.xml.stream.XMLStreamReader;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 * 
 */
public abstract class AbstractStAXExpression extends Expression {

    protected final String name;
    protected final XMLStreamReader reader;

    public AbstractStAXExpression(XMLStreamReader reader) throws Throwable {
        this(reader, null);
    }

    public AbstractStAXExpression(XMLStreamReader reader, String name) throws Throwable {
        this(new StreamGraphBuilder(), reader, name);
    }

    public AbstractStAXExpression(GraphBuilder builder, XMLStreamReader reader) throws Throwable {
        this(builder, reader, null);
    }

    public AbstractStAXExpression(GraphBuilder builder, XMLStreamReader reader, String name) throws Throwable {
        super(builder);
        this.name = name;
        this.reader = reader;
        relationship = builder.build(this);
    }

    protected String qname(QName qname) {
        if (qname.getPrefix().isEmpty())
            return qname.getLocalPart();
        StringBuilder s = new StringBuilder();
        s.append(qname.getPrefix()); s.append(":"); s.append(qname.getLocalPart());
        return s.toString();
    }

    protected Object[][] _(Object[]... o) {
        return o;
    }

    protected Object[] _(Statement s, Object o) {
        Object[] a = {s, o};
        return a;
    }

    protected Object[] name(String name) {
        return _(QNAME._, name);
    }

    protected Object[] value(Object value) {
        return _(VALUE._, value);
    }

}
