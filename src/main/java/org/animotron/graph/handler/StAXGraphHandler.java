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
package org.animotron.graph.handler;

import org.animotron.statement.Statement;
import org.animotron.statement.ml.*;
import org.animotron.statement.value.VALUE;
import org.neo4j.graphdb.Relationship;

import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;
import java.io.IOException;

/**
 * @author <a href="mailto:gazdovskyd@gmail.com">Evgeny Gazdovsky</a>
 * 
 */
public class StAXGraphHandler implements GraphHandler {
	
	private XMLStreamWriter writer;

	public StAXGraphHandler(XMLStreamWriter writer) {
		this.writer = writer;
	}
	
	@Override
	public void start(Statement statement, Statement parent, Relationship r, int level, boolean isOne, int pos, boolean isLast) throws IOException {
	}

	@Override
	public void end(Statement statement, Statement parent, Relationship r, int level, boolean isOne, int pos, boolean isLast) throws IOException {
	}

    @Override
    public void start(Statement statement, Statement parent, Object[] param, int level, boolean isOne, int pos, boolean isLast) throws IOException {
        try {
            if (statement instanceof ATTRIBUTE) {
                writer.writeAttribute(param[0].toString(), param[1].toString());
            } else if (statement instanceof ENTITY){
                writer.writeEntityRef(param[0].toString());
            } else if (statement instanceof ELEMENT) {
                writer.writeStartElement(param[0].toString());
            } else if (statement instanceof PI) {
                if (param[1] == null) {
                    writer.writeProcessingInstruction (param[0].toString());
                } else {
                    writer.writeProcessingInstruction (param[0].toString(), param[1].toString());
                }
            } else if (statement instanceof NS) {
                writer.writeNamespace(param[0].toString(), param[1].toString());
            }
        } catch (XMLStreamException e) {
            throw new IOException(e);
        }
    }

    @Override
    public void end(Statement statement, Statement parent, Object[] param, int level, boolean isOne, int pos, boolean isLast) throws IOException {
        try {
            if (statement instanceof ELEMENT) {
                writer.writeEndElement();
            }
        } catch (XMLStreamException e) {
            throw new IOException(e);
        }
    }

    @Override
    public void start(Statement statement, Statement parent, Object param, int level, boolean isOne, int pos, boolean isLast) throws IOException {
        try {
            if (statement instanceof COMMENT){
                writer.writeComment(param.toString());
            } else if (statement instanceof CDATA){
                writer.writeCData(param.toString());
            } else if (statement instanceof DTD){
                writer.writeDTD(param.toString());
            } else if (statement instanceof VALUE) {
                writer.writeCharacters(param.toString());
            }

        } catch (XMLStreamException e) {
            throw new IOException(e);
        }
    }

    @Override
    public void end(Statement statement, Statement parent, Object param, int level, boolean isOne, int pos, boolean isLast) throws IOException {
    }

    @Override
	public void startGraph() throws IOException {
		try {
			writer.writeStartDocument();
		} catch (XMLStreamException e) {
            throw new IOException(e);
		}
	}

	@Override
	public void endGraph() throws IOException {
		try {
			writer.writeEndDocument();
		} catch (XMLStreamException e) {
            throw new IOException(e);
		}
	}

	private boolean stepMade = false;
	
	public void stepMade() {
		stepMade = true;
	}

	public boolean isStepMade() {
		return stepMade;
	}
}