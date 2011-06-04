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
package org.animotron.graph.sax;

import org.animotron.graph.GraphBuilder;
import org.animotron.instruction.Instruction;
import org.animotron.instruction.ml.ATTRIBUTE;
import org.animotron.instruction.ml.CDATA;
import org.animotron.instruction.ml.COMMENT;
import org.animotron.instruction.ml.TEXT;
import org.xml.sax.Attributes;
import org.xml.sax.ContentHandler;
import org.xml.sax.Locator;
import org.xml.sax.SAXException;
import org.xml.sax.ext.LexicalHandler;


/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 * 
 */
public class SAXGraphBuilder extends GraphBuilder implements ContentHandler, LexicalHandler {
	
	private Instruction valueStatement = TEXT.getInstance();
	
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
		start(tmp[0], ns, tmp[1], null);
		for (int i = 0; i < attributes.getLength(); i++) {
			if ("CDATA".equals(attributes.getType(i))) {
				tmp = qname(attributes.getLocalName(i), attributes.getQName(i));
				start(ATTRIBUTE.getInstance(), 
						tmp[0], attributes.getURI(i), tmp[1], attributes.getValue(i));
				end();
			}
		}

    }	
	
	@Override
	public void endElement (String ns, String name, String qname) throws SAXException {
    	end();
	}
	
	@Override
    public void characters (char ch[], int start, int length) throws SAXException {
		String value = new String(ch, start, length);
		if (valueStatement instanceof TEXT) {
			value = removeWS(value);
			if (value != null) {
				start(valueStatement, null, null, null, value);
				end();
			}
		} else {
			start(valueStatement, null, null, null, value);
			end();
		}
    }

	@Override
	public void comment(char ch[], int start, int length) throws SAXException {
		start(COMMENT.getInstance(), null, null, null, new String(ch, start, length));
		end();
	}

	@Override
	public void startCDATA() throws SAXException {
		valueStatement = CDATA.getInstance();
	}

	@Override
	public void endCDATA() throws SAXException {
		valueStatement = TEXT.getInstance();
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

	@Override
	public void endPrefixMapping(String arg0) throws SAXException {
		// TODO Auto-generated method stub
	}

	@Override
	public void ignorableWhitespace(char[] arg0, int arg1, int arg2) throws SAXException {
		// TODO Auto-generated method stub
	}

	@Override
	public void processingInstruction(String arg0, String arg1) throws SAXException {
		// TODO Auto-generated method stub
	}

	@Override
	public void setDocumentLocator(Locator arg0) {
		// TODO Auto-generated method stub
	}

	@Override
	public void skippedEntity(String arg0) throws SAXException {
		// TODO Auto-generated method stub
	}

	@Override
	public void startPrefixMapping(String arg0, String arg1) throws SAXException {
		// TODO Auto-generated method stub
	}
	
}
