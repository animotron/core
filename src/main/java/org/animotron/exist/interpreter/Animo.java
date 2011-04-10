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
import org.exist.dom.NodeProxy;
import org.exist.dom.NodeSet;
import org.exist.dom.QName;
import org.exist.memtree.NodeImpl;
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

			if (ns.equals(Namespaces.AN.namespace())) {

				String name = input.getLocalName();

				if (name.equals(Keywords.AN_EMPTY.keyword())) {
					// TODO: process an:empty

				} else if (name.equals(Keywords.AN_SELF.keyword())) {
					// TODO: process an:self

				} else if (name.equals(Keywords.AN_CONTENT.keyword())) {
					// TODO: process an:content

				} else {
					// TODO: process an:*
				}

			} else if (ns.equals(Namespaces.ANY.namespace())) {
				// TODO: process any:*

			} else if (ns.equals(Namespaces.ALL.namespace())) {
				// TODO: process all:*

			} else if (ns.equals(Namespaces.PTRN.namespace())) {
				// TODO: process ptrn:*

			} else if (ns.equals(Namespaces.GET.namespace())) {
				// TODO: process get:*

			} else if (ns.equals(Namespaces.SELF.namespace())) {

				if (input.getLocalName().equals(
						Keywords.SELF_INSTANCE.keyword())) {
					// process self:instance 
					// return local-name() as text()
				} else {
					// TODO: process self:*

				}

			} else if (ns.equals(Namespaces.IC.namespace())) {
				// skip ic:* 
				// return input as is
				return (NodeSet) input;

			} else {

				QName keyword = input.getQName();

				if (keyword.equals(Keywords.DO_SKIP.QName())) {
					// TODO: process do:skip

				} else if (keyword.equals(Keywords.DO_XQUERY.QName())) {
					// TODO: process do:xquery

				} else if (keyword.equals(Keywords.DO_XSLT.QName())) {
					// TODO: process do:xslt

				} else if (keyword.equals(Keywords.USE_FLOW_STACK.QName())) {
					// TODO: process use:flow-stack

				} else if (keyword.equals(Keywords.USE_CONTEXT_STACK.QName())) {
					// TODO: process use:stack

				} else if (keyword.equals(Keywords.USE_LOCAL_CONTEXT.QName())) {
					// TODO: process use:context

				} else if (keyword.equals(Keywords.USE_CONTEXT.QName())) {
					// TODO: process use:CONTEXT

				} else if (keyword.equals(Keywords.USE_GLOBAL_CONTEXT.QName())) {
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
