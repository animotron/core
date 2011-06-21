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

import static org.neo4j.graphdb.Direction.OUTGOING;

import org.animotron.Statement;
import org.animotron.graph.GraphHandler;
import org.animotron.instruction.ml.ATTRIBUTE;
import org.animotron.instruction.ml.CDATA;
import org.animotron.instruction.ml.COMMENT;
import org.animotron.instruction.ml.ELEMENT;
import org.animotron.instruction.ml.TEXT;
import org.animotron.instruction.ml.ValueInstruction;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.RelationshipType;
import org.xml.sax.ContentHandler;
import org.xml.sax.SAXException;
import org.xml.sax.ext.LexicalHandler;
import org.xml.sax.helpers.AttributesImpl;

/**
 * @author <a href="mailto:gazdovskyd@gmail.com">Evgeny Gazdovsky</a>
 * 
 */
public class SAXGraphSerializer implements GraphHandler {
	
	private static final ATTRIBUTE ATTR = ATTRIBUTE._;
	private static final RelationshipType ATTR_RELATIONSHIPTYPE = ATTR.relationshipType();
	private ContentHandler contentHandler;
	private LexicalHandler lexicalHandler;
	
	public SAXGraphSerializer(ContentHandler c, LexicalHandler l){
		contentHandler = c;
		lexicalHandler = l;
	}

	@Override
	public void start(Statement statement, Relationship r) {
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
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	@Override
	public void end(Statement statement, Relationship r) {
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
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	@Override
	public void startGraph() {
		try {
			contentHandler.startDocument();
		} catch (SAXException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	@Override
	public void endGraph() {
		try {
			contentHandler.endDocument();
		} catch (SAXException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	
}
