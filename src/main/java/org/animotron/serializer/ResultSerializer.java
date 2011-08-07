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

import org.animotron.Statement;
import org.animotron.instruction.ml.*;
import org.animotron.operator.Result;
import org.codehaus.stax2.XMLStreamWriter2;
import org.neo4j.graphdb.Relationship;

import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;

/**
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class ResultSerializer extends AnimoResultSerializer {

    public ResultSerializer(XMLStreamWriter writer) {
        super(writer);
    }

    @Override
    public void start(Statement statement, Relationship r) {
        if (statement instanceof Result) {
            super.start(statement, r);
        }
    }

    @Override
    public void end(Statement statement, Relationship r) {
        if (statement instanceof Result) {
            super.end(statement, r);
        }
    }

}