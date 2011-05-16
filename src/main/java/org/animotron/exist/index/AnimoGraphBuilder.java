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

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.Stack;

import org.animotron.Keywords;
import org.animotron.Namespaces;
import org.animotron.Sources;
import org.apache.log4j.Logger;
import org.exist.dom.QName;
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
	private Set<Integer> predicates = new HashSet<Integer>();
	Map<Integer, Sources> sources = new HashMap<Integer, Sources>();
	
	private int skip_level, element_level;
	private int level = 0;
	private boolean _animo_, _skip_, _element_;
	private Transaction tx;
	
	private void push_element(){
		element_level = level; _element_ = true;
	}
	
	private void push_skip(){
		skip_level = level; _skip_ = true;
	}
	
	private void push_source(Sources source){
		sources.put(level, source);
	}
	
	private Sources source(){
		return sources.get(level-1);
	}
	
	private void push_predicat(){
		predicates.add(level+1);
	}
	
	private boolean predicat(){
		return predicates.contains(level);
	}
	
	public void startElement(Element element) {
		
		if (level == 0 ){
			current = active = the = null;
			skip_level = element_level = 0;
			_animo_ = true; _skip_ =  _element_ = false;
		}
		
		level++;
		
		if (!_animo_ || _skip_)
			return;
		
		if (_element_ && level <= element_level) {
			element_level = 0; _element_ = false;
		}

		if (_skip_ && level <= skip_level) { 
			skip_level = 0; _skip_ = false;
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
					current = AnimoGraph.createElement(current, element);
				} else {
					if (Namespaces.AN.equals(ns)) {
						active = AnimoGraph.createAN(current, name, source());
						current = active;
					} else if (Namespaces.ANY.equals(ns)) {
						active = AnimoGraph.createANY(current, name, source());
						current = active;
						push_predicat();
					}  else if (Namespaces.ALL.equals(ns)) {
						active = AnimoGraph.createALL(current, name, source());
						current = active;
						push_predicat();
					} else if (Namespaces.GT.equals(ns)) {
						if (predicat()) {
							active = AnimoGraph.createGT(current, name);
						} else {
							active = AnimoGraph.createGT(current, name, source());
						}
						current = active;
					} else if (Namespaces.GE.equals(ns)) {
						if (predicat()) {
							active = AnimoGraph.createGE(current, name);
						} else {
							active = AnimoGraph.createGE(current, name, source());
						}
						current = active;
					} else if (Namespaces.EQ.equals(ns)) {
						if (predicat()) {
							active = AnimoGraph.createEQ(current, name);
						} else {
							active = AnimoGraph.createEQ(current, name, source());
						}
						current = active;
					} else if (Namespaces.NE.equals(ns)) {
						if (predicat()) {
							active = AnimoGraph.createNE(current, name);
						} else {
							active = AnimoGraph.createNE(current, name, source());
						}
						current = active;
					} else if (Namespaces.LE.equals(ns)) {
						if (predicat()) {
							active = AnimoGraph.createLE(current, name);
						} else {
							active = AnimoGraph.createLE(current, name, source());
						}
						current = active;
					} else if (Namespaces.LT.equals(ns)) {
						if (predicat()) {
							active = AnimoGraph.createLT(current, name);
						} else {
							active = AnimoGraph.createLT(current, name, source());
						}
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
						push_element();
					} else if (Keywords.DO_XSLT.equals(qname)) {
						active = AnimoGraph.createXSLT(current);
						current = active;
						push_element();
					} else if (the != null && Namespaces.IS.equals(ns) && level == 2) {
						AnimoGraph.addIsRelationship(the, name);
						push_skip();
					} else if (Namespaces.USE.equals(ns)) {
						if (Keywords.USE_GLOBAL_CONTEXT.keyword().equals(name)){
							push_source(Sources.GLOBAL_CONTEXT);
						} else if (Keywords.USE_CONTEXT.keyword().equals(name)){
							push_source(Sources.CONTEXT);
						} else if (Keywords.USE_LOCAL_CONTEXT.keyword().equals(name)){
							push_source(Sources.LOCAL_CONTEXT);
						} else if (Keywords.USE_CONTEXT_STACK.keyword().equals(name)){
							push_source(Sources.CONTEXT_STACK);
						} else if (Keywords.USE_FLOW_STACK.keyword().equals(name)){
							push_source(Sources.FLOW_STACK);
						} else if (the != null && level == 2) {
							AnimoGraph.addUseRelationship(the, name);
						} else {
							AnimoGraph.addUseRelationship(active, name);
							push_skip();
						}
					} else {
						current = AnimoGraph.createElement(current, element);
					}
				}
			} catch (Exception e) {
				tx.finish();
				LOG.error("AnimoGraph build error for element \"" + element.getNodeName() + "\"" , e);
			}
		}
	}

	public void endElement(Element element) {
		if (!_animo_ || _skip_ || sources.containsKey(level--)) 
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
			AnimoGraph.createAttribute(current, attribute);
		} catch (Exception e) {
			tx.finish();
			LOG.error("AnimoGraph build error for attribute " + attribute.getNodeName() + " = \"" + attribute.getNodeValue() + "\"" , e);
		}
	}

	public void characters(CharacterData text) {
		if (!_animo_ || _skip_)
			return;
		
		try {
			AnimoGraph.createCharacterData(current, text);
		} catch (Exception e) {
			tx.finish();
			LOG.error("AnimoGraph build error for node \"" + text.getNodeValue() + "\"" , e);
		}
	}

}
