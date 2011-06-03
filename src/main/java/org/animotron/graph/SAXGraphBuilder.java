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
	
	private String[] qname (String name, String qname){
		String[] tmp = {null, name};
		if (qname != null) {
			int colon = qname.indexOf(":");
			if (colon > 0) {
				tmp[0] = qname.substring(0, colon); 
				tmp[1] = qname.substring(colon+1); 
			}
		}
		return tmp;
	}
	
	@Override
	public void startElement (String ns, String name, String qname, Attributes attributes) throws SAXException {
		String[] tmp = qname(name, qname);
		builder.start(tmp[0], ns, tmp[1], null);
		for (int i = 0; i < attributes.getLength(); i++) {
			if ("CDATA".equals(attributes.getType(i))) {
				tmp = qname(attributes.getLocalName(i), attributes.getQName(i));
				builder.start(ATTRIBUTE.getInstance(), 
						tmp[0], attributes.getURI(i), tmp[1], attributes.getValue(i));
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
		builder.start(value, null, null, null, new String(ch, start, length));
		builder.end();
    }

	@Override
	public void comment(char ch[], int start, int length) throws SAXException {
		builder.start(COMMENT.getInstance(), null, null, null, new String(ch, start, length));
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
