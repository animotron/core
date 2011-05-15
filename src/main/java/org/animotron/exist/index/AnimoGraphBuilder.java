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
import org.exist.dom.ElementAtExist;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Transaction;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * 
 */
public class AnimoGraphBuilder {

	private static final Logger LOG = Logger.getLogger(AnimoGraphBuilder.class);

	// here because of optimization reasons
	private Node THENode, activeNode;

	private Stack<Node> nodes = new Stack<Node>();
	
	private int skip;
	private int level = 0;
	private boolean animo;
	
	private Transaction tx;
	
	public void startElement(ElementAtExist element) {
		
		if (level == 0 ){
			System.out.println("reset");
			skip = 0;
			animo = true;
			THENode = null;
			activeNode = null;
		}
		
		level++;
		
		if (!animo || (skip > 0 && level - 1 > skip))
			return;
		
		if (level <= skip)
			skip = 0;
		
		String ns = element.getNamespaceURI();
		
		if (level == 1) {
			if (Namespaces.THE.equals(ns)) {
				tx = AnimoGraph.beginTx();
				THENode = AnimoGraph.getOrCreateTHE(element);
				activeNode = THENode;
				animo = true;
			} else {
				animo = false;
			}
		} else {
			nodes.push(activeNode);
			try {
				if (THENode != null && Namespaces.IS.equals(ns) && level == 2) {
					AnimoGraph.addIsRelationship(THENode, AnimoGraph.getOrCreateTHE(element));
					skip = level;
				//} else if (Namespaces.USE.equals(ns)) {
				//	if (THENode != null && level == 2) {
				//		AnimoGraph.addUseRelationship(THENode, AnimoGraph.getOrCreateTHE(element));
				//	} else {
				//	}
				} else {
					activeNode = AnimoGraph.createExistNode(activeNode, element);
				}
			} catch (Exception e) {
				tx.finish();
				LOG.error("AnimoGraph build error for element \"" + element.getNodeName() + "\"" , e);
			}
		}
	}

	public void endElement(ElementAtExist element) {
		
		level--;
		if (!animo || (skip > 0 && level + 1 >= skip)) 
			return;

		try {
			if (level > 0) {
				activeNode = nodes.pop();
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

}
