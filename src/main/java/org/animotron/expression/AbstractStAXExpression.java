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

import org.animotron.statement.Statement;

import javax.xml.namespace.QName;
import javax.xml.stream.XMLStreamReader;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 * 
 */
public abstract class AbstractStAXExpression extends AbstractExpression {

    protected final String name;
    protected final XMLStreamReader reader;

    public AbstractStAXExpression(XMLStreamReader reader) {
        this(reader, null);
    }

    public AbstractStAXExpression(XMLStreamReader reader, String name) {
        this.name = name;
        this.reader = reader;
    }

    protected String qname(QName qname) {
        if (qname.getPrefix().isEmpty())
            return qname.getLocalPart();
        StringBuilder s = new StringBuilder();
        s.append(qname.getPrefix()); s.append(":"); s.append(qname.getLocalPart());
        return s.toString();
    }

    protected Object[] _(Statement s, Object o) {
        Object[] a = {s, o};
        return a;
    }

}
