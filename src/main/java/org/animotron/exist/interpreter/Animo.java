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

import org.exist.dom.NewArrayNodeSet;
import org.exist.dom.NodeProxy;
import org.exist.dom.NodeSet;
import org.exist.xquery.XPathException;
import org.exist.xquery.value.SequenceIterator;
import org.exist.xquery.value.Type;
import org.w3c.dom.Node;

/**
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 * 
 */
public class Animo extends Process {

	public NodeSet process(NodeSet flow) throws XPathException {
		return process(flow, null);
	}

	public NodeSet process(NodeSet flow, NodeSet context) throws XPathException {
		return process(flow, null, context, null, null);
	}

	protected NodeSet process(NodeSet flow, NodeSet stackFlow, NodeSet context,
			NodeSet stackContext, NodeSet source) throws XPathException {
		NodeSet res = new NewArrayNodeSet();
		SequenceIterator i = flow.iterate();
		while (i.hasNext()) {
			NodeProxy input = (NodeProxy) i.nextItem();
			NodeSet set = process(input, stackFlow, context, stackContext,
					source);
			res.addAll(set);
		}
		return res;
	}

	private NodeSet process(NodeProxy input, NodeSet stackFlow,
			NodeSet context, NodeSet stackContext, NodeSet source)
			throws XPathException {

		switch (input.getType()) {

			case Type.ELEMENT: {
				 
				Node node = input.getNode();
				
				return process(node, stackFlow, context, stackContext, source);
				
			}
	
			default:
				return input;
				
		}
	}

	@Override
	protected NodeSet process(Node input, NodeSet stackFlow,
			NodeSet context, NodeSet stackContext, NodeSet source)
			throws XPathException {
		// TODO Auto-generated method stub
		return null;
	}

}
