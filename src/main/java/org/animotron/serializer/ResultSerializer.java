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

import java.io.IOException;
import java.io.OutputStream;

import org.animotron.Statement;
import org.animotron.operator.Result;
import org.neo4j.graphdb.Relationship;

import com.ctc.wstx.stax.WstxOutputFactory;

import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;

/**
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class ResultSerializer extends AnimoResultSerializer {
	
	public static final WstxOutputFactory OUTPUT_FACTORY = new WstxOutputFactory();

	public static void serialize(Relationship r, OutputStream out) throws IOException, InterruptedException {
        XMLStreamWriter writer;
        try {
            writer = OUTPUT_FACTORY.createXMLStreamWriter(out);
        } catch (XMLStreamException e) {
            throw new IOException(e);
        }

		ResultSerializer serializer = new ResultSerializer(writer);
        serializer.serialize(r, r);
	}

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