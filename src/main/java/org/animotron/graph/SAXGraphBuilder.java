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
package org.animotron.graph;

import org.animotron.Quanta;
import org.animotron.Statement;
import org.animotron.Statements;
import org.animotron.instruction.Instruction;
import org.animotron.instruction.InstructionContainer;
import org.animotron.instruction.ml.ATTRIBUTE;
import org.animotron.instruction.ml.CDATA;
import org.animotron.instruction.ml.COMMENT;
import org.animotron.instruction.ml.ELEMENT;
import org.animotron.instruction.ml.TEXT;
import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.ext.LexicalHandler;
import org.xml.sax.helpers.DefaultHandler;


/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 * 
 */
public class SAXGraphBuilder extends DefaultHandler implements LexicalHandler {
	
	private Instruction value = TEXT.getInstance();
	
	private GraphBuilder builder;
	
	public SAXGraphBuilder(GraphBuilder builder) {
		this.builder = builder;
	}
	
	@Override
	public void startDocument() {
		builder.startDocument();
	}
	
	@Override
	public void endDocument() {
		builder.endDocument();
	}
	
	@Override
	public void startElement (String ns, String name, String qname, Attributes attributes) throws SAXException {
		Statement statement;
		Quanta container = Statements.namespace(ns);
		if (container instanceof InstructionContainer) {
			statement = ((InstructionContainer) container).getInstruction(name);
		} else {
			statement = (Statement) container;
		}
		if (statement == null) {
			statement = ELEMENT.getInstance();
			name = qname;
		}
		builder.start(statement, ns, name, null);
		for (int i = 0; i < attributes.getLength(); i++) {
			if ("CDATA".equals(attributes.getType(i))) {
				builder.start(ATTRIBUTE.getInstance(), attributes.getURI(i), attributes.getQName(i), attributes.getValue(i));
				builder.end();
			}
		}

    }	
	
	@Override
	public void endElement (String ns, String name, String qname) throws SAXException {
    	builder.end();
	}
	
	@Override
    public void characters (char ch[], int start, int length) throws SAXException {
		builder.start(value, null, null, new String(ch, start, length));
		builder.end();
    }

	@Override
	public void comment(char ch[], int start, int length) throws SAXException {
		builder.start(COMMENT.getInstance(), null, null, new String(ch, start, length));
		builder.end();
	}

	@Override
	public void startCDATA() throws SAXException {
		value = CDATA.getInstance();
	}

	@Override
	public void endCDATA() throws SAXException {
		value = TEXT.getInstance();
	}

	@Override
	public void endDTD() throws SAXException {
		// TODO Auto-generated method stub
	}

	@Override
	public void endEntity(String arg0) throws SAXException {
		// TODO Auto-generated method stub
	}

	@Override
	public void startDTD(String arg0, String arg1, String arg2) throws SAXException {
		// TODO Auto-generated method stub
	}

	@Override
	public void startEntity(String arg0) throws SAXException {
		// TODO Auto-generated method stub
	}
	
}
