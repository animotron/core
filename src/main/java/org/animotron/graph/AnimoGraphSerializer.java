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

import org.animotron.Properties;
import org.neo4j.graphdb.Direction;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.RelationshipType;
import org.xml.sax.Attributes;
import org.xml.sax.ContentHandler;
import org.xml.sax.SAXException;
import org.xml.sax.ext.LexicalHandler;
import org.xml.sax.helpers.AttributesImpl;

/**
 * @author <a href="mailto:gazdovskyd@gmail.com">Evgeny Gazdovsky</a>
 * 
 */
public class AnimoGraphSerializer {
	
	ContentHandler contentHandler;
	LexicalHandler lexicalHandler;

	public AnimoGraphSerializer(ContentHandler contentHandler, LexicalHandler lexicalHandler) {
		this.contentHandler = contentHandler;
		this.lexicalHandler = lexicalHandler;
	}
	
	private Attributes attributes(Node node) {
		AttributesImpl res = new AttributesImpl(); 
		for (Relationship r : node.getRelationships(RelationshipTypes.ATTRIBUTE, Direction.OUTGOING)){
			Node attr = r.getEndNode();
			res.addAttribute(
					Properties.NAMESPACE.get(attr), null, 
					Properties.NAME.get(attr), null, 
					Properties.VALUE.get(attr)
				);
		}
		return res;
	}
	
	private void element(Keywords keyword, Relationship r) throws SAXException {
		element(keyword.namespace(), keyword.QName().getStringValue(), r.getEndNode());
	}
	
	private void element(Namespaces ns, Relationship r) throws SAXException {
		Node node = r.getEndNode();
		Node ref = node.getSingleRelationship(RelationshipTypes.REF, Direction.OUTGOING).getEndNode(); 
		element(ns.namespace(), Properties.NAME.get(ref), node);
	}

	private void element(Namespaces ns, Node node) throws SAXException{
		element(ns.namespace(), Properties.NAME.get(node), node);
	}

	private void element(Node node) throws SAXException{
		element(Properties.NAMESPACE.get(node), Properties.NAME.get(node), node);
	}

	private void element(String ns, String name, Node node) throws SAXException {
		contentHandler.startElement(ns, null, name, attributes(node));
		for (Relationship r : node.getRelationships(Direction.OUTGOING)) 
			serialize(r);
		contentHandler.endElement(ns, null, name);
	}

	private void text(Node node) throws SAXException{
		String text = Properties.VALUE.get(node);
		contentHandler.characters(text.toCharArray(), 0, text.length());
	}

	private void comment(Node node) throws SAXException{
		String text = Properties.VALUE.get(node);
		lexicalHandler.comment(text.toCharArray(), 0, text.length());
	}

	private void cdata(Node node) throws SAXException{
		lexicalHandler.startCDATA();
		text(node);
		lexicalHandler.endCDATA();
	}

	public void serialize(Relationship r) throws SAXException{
		
		RelationshipType type = r.getType();
		
		if (AnimoRelationshipType.isSupertypeOf(type)) {
			element(Namespaces.THE, r.getEndNode());
			
		} else if (RelationshipTypes.IS.equals(type)){
			element(Namespaces.IS, r.getEndNode());
		} else if (RelationshipTypes.USE.equals(type)){
			element(Namespaces.USE, r.getEndNode());
			
		} else if (RelationshipTypes.AN.equals(type)){
			element(Namespaces.AN, r);
		} else if (RelationshipTypes.ANY.equals(type)){
			element(Namespaces.ANY, r);
		} else if (RelationshipTypes.ALL.equals(type)){
			element(Namespaces.ALL, r);
			
		} else if (RelationshipTypes.PTRN.equals(type)){
			element(Namespaces.PTRN, r);
			
		} else if (RelationshipTypes.HAVE.equals(type)){
			element(Namespaces.HAVE, r);
		} else if (RelationshipTypes.IC.equals(type)){
			element(Namespaces.IC, r);
			
		} else if (RelationshipTypes.GET.equals(type)){
			element(Namespaces.GET, r);
		} else if (RelationshipTypes.SELF.equals(type)){
			element(Namespaces.SELF, r);
			
		} else if (RelationshipTypes.GT.equals(type)){
			element(Namespaces.GT, r);
		} else if (RelationshipTypes.GE.equals(type)){
			element(Namespaces.GE, r);
		} else if (RelationshipTypes.EQ.equals(type)){
			element(Namespaces.EQ, r);
		} else if (RelationshipTypes.NE.equals(type)){
			element(Namespaces.NE, r);
		} else if (RelationshipTypes.LE.equals(type)){
			element(Namespaces.LE, r);
		} else if (RelationshipTypes.LT.equals(type)){
			element(Namespaces.LT, r);
			
		} else if (RelationshipTypes.XQUERY.equals(type)){
			element(Keywords.DO_XQUERY, r);
		} else if (RelationshipTypes.XSLT.equals(type)){
			element(Keywords.DO_XSLT, r);
			
		} else if (RelationshipTypes.ELEMENT.equals(type)){
			element(r.getEndNode());
		} else if (RelationshipTypes.TEXT.equals(type)){
			text(r.getEndNode());
		} else if (RelationshipTypes.COMMENT.equals(type)){
			comment(r.getEndNode());
		} else if (RelationshipTypes.CDATA.equals(type)){
			cdata(r.getEndNode());
		}
		
	}
		
}
