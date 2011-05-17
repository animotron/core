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
package org.animotron.exist.index;

import java.util.HashSet;
import java.util.Set;
import java.util.Stack;
import java.util.StringTokenizer;

import org.animotron.Keywords;
import org.animotron.Namespaces;
import org.apache.log4j.Logger;
import org.exist.dom.QName;
import org.exist.xquery.value.Type;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Transaction;
import org.w3c.dom.Attr;
import org.w3c.dom.CharacterData;
import org.w3c.dom.Element;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * 
 */
public class AnimoGraphBuilder {
	
	private static final Logger LOG = Logger.getLogger(AnimoGraphBuilder.class);

	// here because of optimization reasons
	private Node the, current, active;

	private Stack<Node> nodes = new Stack<Node>();
	private Set<Node> predicates = new HashSet<Node>();
	
	private int skip_level, element_level;
	private int level = 0;
	private boolean _animo_, _skip_, _element_;
	private Transaction tx;
	
	private void pushElement(){
		element_level = level; _element_ = true;
	}
	
	private void pushSkip(){
		skip_level = level; _skip_ = true;
	}
	
	private void pushPredicat(Node node){
		predicates.add(node);
	}
	
	private boolean hasPredicat(Node node){
		return predicates.contains(node);
	}
	
	public void startElement(Element element) {
		
		level++;
		
		if (level == 1 ){
			current = active = the = null;
			skip_level = element_level = 0;
			_animo_ = true; _skip_ =  _element_ = false;
		} else {
			if (_skip_ && level <= skip_level) { 
				skip_level = 0; _skip_ = false;
			}
		}
		
		if (!_animo_ || _skip_)
			return;
		
		if (_element_ && level <= element_level) {
			element_level = 0; _element_ = false;
		}

		String ns = element.getNamespaceURI();
		String name = element.getLocalName();
		QName qname = new QName(name, ns);
		
		if (level == 1) {
			if (Namespaces.THE.equals(ns)) {
				tx = AnimoGraph.beginTx();
				the = AnimoGraph.getTHE(name);
				if (the == null){
					the = AnimoGraph.createTHE(name);
				} else {
					AnimoGraph.clear(the);
				}
				current = the;
				_animo_ = true;
			} else {
				_animo_ = false;
			}
		} else {
			nodes.push(current);
			try {
				if (_element_) {
					current = AnimoGraph.createElement(current,  element.getLocalName(), element.getNamespaceURI(), element.getPrefix());
				} else if (hasPredicat(active)) {
					if (Namespaces.GT.equals(ns)) {
						active = AnimoGraph.createGT(current, name);
						current = active;
					} else if (Namespaces.GE.equals(ns)) {
						active = AnimoGraph.createGE(current, name);
						current = active;
					} else if (Namespaces.EQ.equals(ns)) {
						active = AnimoGraph.createEQ(current, name);
						current = active;
					} else if (Namespaces.NE.equals(ns)) {
						active = AnimoGraph.createNE(current, name);
						current = active;
					} else if (Namespaces.LE.equals(ns)) {
						active = AnimoGraph.createLE(current, name);
						current = active;
					} else {
						pushSkip();
					}
				} else if (Namespaces.LT.equals(ns)) {
					if (hasPredicat(active)) {
						active = AnimoGraph.createLT(current, name);
						current = active;
					} else {
						pushSkip();
					}
				} else {
					if (Namespaces.AN.equals(ns)) {
						active = AnimoGraph.createAN(current, name, element.getAttribute(AnimoGraph.NAME));
						current = active;
					} else if (Namespaces.ANY.equals(ns)) {
						active = AnimoGraph.createANY(current, name, element.getAttribute(AnimoGraph.NAME));
						pushPredicat(active);
						current = active;
					}  else if (Namespaces.ALL.equals(ns)) {
						active = AnimoGraph.createALL(current, name, element.getAttribute(AnimoGraph.NAME));
						pushPredicat(active);
						current = active;
					} else if (Namespaces.PTRN.equals(ns)) {
						active = AnimoGraph.createPTRN(current, name);
						current = active;
					} else if (Namespaces.HAVE.equals(ns)) {
						active = AnimoGraph.createHAVE(current, name);
						current = active;
					} else if (Namespaces.IC.equals(ns)) {
						active = AnimoGraph.createIC(current, name);
						current = active;
					} else if (Namespaces.GET.equals(ns)) {
						active = AnimoGraph.createGET(current, name);
						current = active;
					} else if (Namespaces.SELF.equals(ns)) {
						active = AnimoGraph.createSELF(current, name);
						current = active;
					} else if (Keywords.DO_XQUERY.equals(qname)) {
						active = AnimoGraph.createXQUERY(current);
						current = active;
						pushElement();
					} else if (Keywords.DO_XSLT.equals(qname)) {
						active = AnimoGraph.createXSLT(current);
						current = active;
						pushElement();
					} else if (the != null && Namespaces.IS.equals(ns) && level == 2) {
						AnimoGraph.addIsRelationship(the, name);
						pushSkip();
					} else if (Namespaces.USE.equals(ns)) {
						if (the != null && level == 2) {
							AnimoGraph.addUseRelationship(the, name);
							pushSkip();
						} else {
							AnimoGraph.addUseRelationship(active, name);
							pushSkip();
						}
					} else {
						current = AnimoGraph.createElement(current,  element.getLocalName(), element.getNamespaceURI(), element.getPrefix());
					}
				}
			} catch (Exception e) {
				tx.finish();
				LOG.error("AnimoGraph build error for element \"" + element.getNodeName() + "\"" , e);
			}
		}
	}

	public void endElement(Element element) {
		level--;
		if (!_animo_ || _skip_) 
			return;
		if (level > 0) {
			current = nodes.pop();
		} else {
			try {
				tx.success();
				tx.finish();
			} catch (Exception e) {
				tx.finish();
				LOG.error("AnimoGraph build error for element \"" + element.getNodeName() + "\"" , e);
			}
		}
	}

	public void attribute(Attr attribute) {
		if (!_animo_ || _skip_)
			return;
		try {
			AnimoGraph.createAttribute(current, attribute.getLocalName(), attribute.getNamespaceURI(), attribute.getPrefix(), attribute.getNodeValue());
		} catch (Exception e) {
			tx.finish();
			LOG.error("AnimoGraph build error for attribute " + attribute.getNodeName() + " = \"" + attribute.getNodeValue() + "\"" , e);
		}
	}

	public void characters(CharacterData text) {
		if (!_animo_ || _skip_)
			return;
		try {
			if (text.getNodeType() == Type.TEXT) {
				String value = text.getNodeValue();
	    		StringBuilder buf = new StringBuilder();
	    		if (value.length() > 0) {
	    			StringTokenizer tok = new StringTokenizer(value);
	    			while (tok.hasMoreTokens()) {
	                    buf.append(tok.nextToken());
	    				if (tok.hasMoreTokens()) buf.append(' ');
	    			}
	    		}
	    		if (buf.length() > 0){
	    			AnimoGraph.createText(current, buf.toString());
	    		}
			} else if (text.getNodeType() == Type.COMMENT) {
				AnimoGraph.createComment(current, text.getNodeValue());
			} else if (text.getNodeType() == Type.CDATA_SECTION) {
				AnimoGraph.createCDATA(current, text.getNodeValue());
			}
		} catch (Exception e) {
			tx.finish();
			LOG.error("AnimoGraph build error for node \"" + text.getNodeValue() + "\"" , e);
		}
	}

}
