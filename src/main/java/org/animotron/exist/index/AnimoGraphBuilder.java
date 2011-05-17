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

import java.util.Stack;

import org.animotron.Namespaces;
import org.apache.log4j.Logger;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Transaction;
import org.w3c.dom.Attr;
import org.w3c.dom.Element;
import org.w3c.dom.CharacterData;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * 
 */
public class AnimoGraphBuilder {

	private static final Logger LOG = Logger.getLogger(AnimoGraphBuilder.class);

	// here because of optimization reasons
	private Node the, current, active;

	private Stack<Node> nodes = new Stack<Node>();
	
	private int skip;
	private int level = 0;
	private boolean animo;
	
	private Transaction tx;
	
	public void startElement(Element element) {
		
		if (level == 0 ){
			//System.out.println("reset");
			animo = true;
			current = null;
			active = null;
			the = null;
			skip = 0;
		}
		
		level++;
		
		if (!animo || (skip > 0 && level - 1 > skip))
			return;
		
		if (level <= skip)
			skip = 0;
		
		String ns = element.getNamespaceURI();
		String name = element.getLocalName();
		
		if (level == 1) {
			if (Namespaces.THE.equals(ns)) {
				tx = AnimoGraph.beginTx();
				the = AnimoGraph.getTHE(name);
				if (the == null){
					the = AnimoGraph.createTHE(name);
				} else {
					//this should be during REMOVE events?
					AnimoGraph.clear(the);
				}
				current = the;
				animo = true;
			} else {
				animo = false;
			}
		} else {
			nodes.push(current);
			try {
				if (Namespaces.AN.equals(ns)) {
					active = AnimoGraph.createAN(current, name);
					current = active;
				}
				if (Namespaces.ANY.equals(ns)) {
					active = AnimoGraph.createANY(current, name);
					current = active;
				}
				if (Namespaces.ALL.equals(ns)) {
					active = AnimoGraph.createALL(current, name);
					current = active;
				}
				if (Namespaces.PTRN.equals(ns)) {
					active = AnimoGraph.createPTRN(current, name);
					current = active;
				}
				if (Namespaces.HAVE.equals(ns)) {
					active = AnimoGraph.createHAVE(current, name);
					current = active;
				}
				if (Namespaces.IC.equals(ns)) {
					active = AnimoGraph.createIC(current, name);
					current = active;
				}
				if (Namespaces.GET.equals(ns)) {
					active = AnimoGraph.createGET(current, name);
					current = active;
				}
				if (Namespaces.SELF.equals(ns)) {
					active = AnimoGraph.createSELF(current, name);
					current = active;
				}
				else if (the != null && Namespaces.IS.equals(ns) && level == 2) {
					AnimoGraph.addIsRelationship(the, name);
					skip = level;
				} else if (Namespaces.USE.equals(ns)) {
					if (the != null && level == 2) {
						AnimoGraph.addUseRelationship(the, name);
					} else {
						AnimoGraph.addUseRelationship(active, name);
					}
				} else {
					current = AnimoGraph.createElement(current, element);
				}
			} catch (Exception e) {
				tx.finish();
				LOG.error("AnimoGraph build error for element \"" + element.getNodeName() + "\"" , e);
			}
		}
	}

	public void endElement(Element element) {
		
		level--;
		if (!animo || (skip > 0 && level + 1 >= skip)) 
			return;

		try {
			if (level > 0) {
				current = nodes.pop();
			}
			if (level == 0) {
				tx.success();
				tx.finish();
			}
		} catch (Exception e) {
			tx.finish();
			LOG.error("AnimoGraph build error for element \"" + element.getNodeName() + "\"" , e);
		}
		
	}

	public void attribute(Attr attribute) {
		
		if (!animo || (skip > 0 && level - 1 > skip))
			return;
		
		try {
			AnimoGraph.createAttribute(current, attribute);
		} catch (Exception e) {
			tx.finish();
			LOG.error("AnimoGraph build error for attribute " + attribute.getNodeName() + " = \"" + attribute.getNodeValue() + "\"" , e);
		}
		
	}

	public void characters(CharacterData text) {
		
		if (!animo || (skip > 0 && level - 1 > skip))
			return;
		
		try {
			AnimoGraph.createCharacterData(current, text);
		} catch (Exception e) {
			tx.finish();
			LOG.error("AnimoGraph build error for node \"" + text.getNodeValue() + "\"" , e);
		}
		
	}

}
