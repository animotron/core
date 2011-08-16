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

import java.io.IOException;

import org.animotron.Statement;
import org.animotron.instruction.ml.*;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.RelationshipType;
import org.xml.sax.ContentHandler;
import org.xml.sax.SAXException;
import org.xml.sax.ext.LexicalHandler;
import org.xml.sax.helpers.AttributesImpl;

import static org.neo4j.graphdb.Direction.OUTGOING;

/**
 * @author <a href="mailto:gazdovskyd@gmail.com">Evgeny Gazdovsky</a>
 * 
 */
public class SAXGraphHandler implements GraphHandler {
	
	private static final ATTRIBUTE ATTR = ATTRIBUTE._;
	private static final RelationshipType ATTR_RELATIONSHIPTYPE = ATTR.relationshipType();
	private ContentHandler contentHandler;
	private LexicalHandler lexicalHandler;
	
	public SAXGraphHandler(ContentHandler c, LexicalHandler l){
		contentHandler = c;
		lexicalHandler = l;
	}

	@Override
	public void start(Statement statement, Relationship r) throws IOException {
		try {
			
			if (statement instanceof TEXT) {
				String value = statement.value(r);
				contentHandler.characters(value.toCharArray(), 0, value.length());
				
			} else if (statement instanceof COMMENT){
				String value = statement.value(r);
				lexicalHandler.comment(value.toCharArray(), 0, value.length());
				
			} else if (statement instanceof CDATA){
				lexicalHandler.startCDATA();
				String value = statement.value(r);
				contentHandler.characters(value.toCharArray(), 0, value.length());
				
			} else if (statement instanceof ATTRIBUTE){
				return;
				
			} else if (statement instanceof ELEMENT){
				String ns = statement.namespace(r);
				String qname = statement.qname(r);
				AttributesImpl attributes = new AttributesImpl();
				for (Relationship i : r.getEndNode().getRelationships(ATTR_RELATIONSHIPTYPE, OUTGOING)){
					attributes.addAttribute(ATTR.namespace(i), null, ATTR.qname(i), "CDATA", ATTR.value(i));
				}
				contentHandler.startElement(ns, null, qname, attributes);
				
			} else {
				String ns = statement.namespace(r);
				String qname = statement.qname(r);
				contentHandler.startElement(ns, null, qname, null);
				
			}
		} catch (SAXException e) {
            throw new IOException(e);
		}
	}

	@Override
	public void end(Statement statement, Relationship r) throws IOException {
		try {
			
			if (statement instanceof CDATA){
				lexicalHandler.endCDATA();
			
			} else if (statement instanceof ValueInstruction) {
				return;
				
			} else {
				String ns = statement.namespace(r);
				String name = statement.name(r);
				contentHandler.endElement(ns, null, name);
				
			}
		} catch (SAXException e) {
            throw new IOException(e);
		}
	}

	@Override
	public void startGraph() throws IOException {
		try {
			contentHandler.startDocument();
		} catch (SAXException e) {
            throw new IOException(e);
		}
	}

	@Override
	public void endGraph() throws IOException {
		try {
			contentHandler.endDocument();
		} catch (SAXException e) {
            throw new IOException(e);
		}
	}
}