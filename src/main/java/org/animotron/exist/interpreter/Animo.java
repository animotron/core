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
package org.animotron.exist.interpreter;

import org.animotron.Keywords;
import org.animotron.Namespaces;
import org.exist.dom.NewArrayNodeSet;
import org.exist.dom.NodeSet;
import org.exist.dom.QName;
import org.exist.memtree.NodeImpl;
import org.exist.memtree.TextImpl;
import org.exist.xquery.XPathException;
import org.exist.xquery.value.SequenceIterator;
import org.exist.xquery.value.Type;
import org.w3c.dom.Node;

/**
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 * 
 */
public class Animo {

	public NodeSet process(NodeSet flow) throws XPathException {
		return process(flow, null);
	}

	public NodeSet process(NodeSet flow, NodeSet context) throws XPathException {
		return process(flow, null, context, null, null);
	}

	private NodeSet process(NodeSet flow, NodeSet stackFlow, NodeSet context,
			NodeSet stackContext, NodeSet source) throws XPathException {
		NodeSet res = new NewArrayNodeSet();
		SequenceIterator i = flow.iterate();
		while (i.hasNext()) {
			NodeImpl input = (NodeImpl) i.nextItem();
			NodeSet set = process(input, stackFlow, context, stackContext,
					source);
			res.addAll(set);
		}
		return res;
	}

	private NodeSet process(NodeImpl input, NodeSet stackFlow, NodeSet context,
			NodeSet stackContext, NodeSet source) throws XPathException {

		switch (input.getType()) {

		case Type.ELEMENT: {

			String ns = input.getNamespaceURI();

			if (Namespaces.AN.equals(ns)) {

				String name = input.getLocalName();

				if (Keywords.AN_EMPTY.keyword().equals(name)) {
					// TODO: process an:empty

				} else if (Keywords.AN_SELF.keyword().equals(name)) {
					// TODO: process an:self

				} else if (Keywords.AN_CONTENT.keyword().equals(name)) {
					// TODO: process an:content

				} else {
					// TODO: process an:*
				}

			} else if (Namespaces.ANY.equals(ns)) {
				// TODO: process any:*

			} else if (Namespaces.ALL.equals(ns)) {
				// TODO: process all:*

			} else if (Namespaces.PTRN.equals(ns)) {
				// TODO: process ptrn:*

			} else if (Namespaces.GET.equals(ns)) {
				// TODO: process get:*

			} else if (Namespaces.SELF.equals(ns)) {

				if (Keywords.SELF_INSTANCE.keyword().equals(input.getLocalName())) {
					// process self:instance 
					// return local-name() as text()
					return (NodeSet) new TextImpl(input.getDocument(), input.getNodeNumber());
					
				} else {
					// TODO: process self:*

				}

			} else if (ns.equals(Namespaces.IC.namespace())) {
				// skip ic:* 
				// return input as is
				return (NodeSet) input;

			} else {

				QName keyword = input.getQName();

				if (Keywords.DO_SKIP.equals(keyword)) {
					// TODO: process do:skip

				} else if (Keywords.DO_XQUERY.equals(keyword)) {
					// TODO: process do:xquery

				} else if (Keywords.DO_XSLT.equals(keyword)) {
					// TODO: process do:xslt

				} else if (Keywords.USE_FLOW_STACK.equals(keyword)) {
					// TODO: process use:flow-stack

				} else if (Keywords.USE_CONTEXT_STACK.equals(keyword)) {
					// TODO: process use:stack

				} else if (Keywords.USE_LOCAL_CONTEXT.equals(keyword)) {
					// TODO: process use:context

				} else if (Keywords.USE_CONTEXT.equals(keyword)) {
					// TODO: process use:CONTEXT

				} else if (Keywords.USE_GLOBAL_CONTEXT.equals(keyword)) {
					// TODO: process use:repository

				} else {
					// process element()
					Node node = input.getNode();
					return process(node, stackFlow, context, stackContext,
							source);
				}

			}
		}

		default:
			// process node()
			return (NodeSet) input;

		}
	}

	private NodeSet process(Node input, NodeSet stackFlow, NodeSet context,
			NodeSet stackContext, NodeSet source) throws XPathException {
		return null;
	}

}
