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
import org.animotron.exist.index.AnimoIndex;
import org.animotron.exist.index.AnimoIndexWorker;
import org.exist.dom.NewArrayNodeSet;
import org.exist.dom.NodeProxy;
import org.exist.dom.NodeSet;
import org.exist.dom.QName;
import org.exist.memtree.MemTreeBuilder;
import org.exist.xquery.XPathException;
import org.exist.xquery.XQueryContext;
import org.exist.xquery.value.Type;

/**
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 * 
 */
public class Animo {
	
	private XQueryContext context;
	
	private Sources source = Sources.GLOBAL_CONTEXT;
	
	public Animo(XQueryContext context) throws XPathException{
		this.context = context;
	}
	
	public NodeSet process(NodeSet flow) throws XPathException {
		return process(flow, null);
	};
	
	public NodeSet process(NodeSet flow, NodeSet context) throws XPathException {
		NodeSet res = new NewArrayNodeSet();
		this.context.pushDocumentContext();
		MemTreeBuilder builder = this.context.getDocumentBuilder();
		process(flow, context, builder);
		res.add(builder.getDocument());
		this.context.popDocumentContext();
		return res;
	}
	
	private NodeSet children(NodeProxy node){
		NodeSet res = new NewArrayNodeSet();;
		for (NodeProxy i : node){
			res.add(i);
		}
		return res;
	}

	private void process(NodeSet input, NodeSet context, MemTreeBuilder builder) throws XPathException {
		for (NodeProxy i : input){
			process(i, context, builder);
		}
	}

	private void process(NodeProxy input, NodeSet context, MemTreeBuilder builder) throws XPathException {

		switch (input.getType()) {

		case Type.ELEMENT: {

			String ns = input.getNode().getNamespaceURI();
			String name = input.getNode().getLocalName();

			if (Namespaces.AN.equals(ns)) {

				if (Keywords.AN_EMPTY.keyword().equals(name)) {
					// process an:empty
					// return nothing
					return;

				} else if (Keywords.AN_SELF.keyword().equals(name)) {
					// process an:self
					// return root
					builder.addReferenceNode(input.getDocument().getFirstChildProxy());

				} else if (Keywords.AN_CONTENT.keyword().equals(name)) {
					// process an:content
					// process children
					process((NodeSet)input, context, builder);

				} else {
					// process reference an:*
					processReference(input, context, builder);
				}

			} else if (Namespaces.ANY.equals(ns)) {
				// TODO: process any:*
				return;

			} else if (Namespaces.ALL.equals(ns)) {
				// TODO: process all:*
				return;

			} else if (Namespaces.PTRN.equals(ns)) {
				// TODO: process ptrn:*
				return;

			} else if (Namespaces.GET.equals(ns)) {
				// TODO: process get:*
				return;

			} else if (Namespaces.SELF.equals(ns)) {

				if (Keywords.SELF_INSTANCE.keyword().equals(name)) {
					// process self:instance 
					// return local name
					builder.characters(name);
					
				} else {
					// TODO: process self:*
					return;

				}

			} else if (Namespaces.IC.equals(ns)) {
				// skip ic:* 
				// return as is 
				builder.addReferenceNode(input);

			} else if (Namespaces.DO.equals(ns)) {
				
				if (Keywords.DO_SKIP.keyword().equals(name)) {
					// process do:skip
					// return children
					for (NodeProxy i : input){
						builder.addReferenceNode(i);
					}

				} else if (Keywords.DO_XQUERY.keyword().equals(name)) {
					// TODO: process do:xquery
					return;

				} else if (Keywords.DO_XSLT.keyword().equals(name)) {
					// TODO: process do:xslt
					return;
					
				}

			} else if (Namespaces.USE.equals(ns)) {
				
				if (Keywords.USE_FLOW_STACK.keyword().equals(name)) {
					// process use:flow-stack
					// process children use flow stack as source of instances
					source = Sources.FLOW_STACK;
					process((NodeSet)input, context, builder);

				} else if (Keywords.USE_CONTEXT_STACK.keyword().equals(name)) {
					// process use:stack
					// process children use context stack as source of instances
					source = Sources.CONTEXT_STACK;
					process((NodeSet)input, context, builder);

				} else if (Keywords.USE_LOCAL_CONTEXT.keyword().equals(name)) {
					// process use:context
					// process children use local context as source of instances
					source = Sources.LOCAL_CONTEXT;
					process((NodeSet)input, context, builder);

				} else if (Keywords.USE_CONTEXT.keyword().equals(name)) {
					// process use:CONTEXT
					// process children use local context and context source as source of instances
					source = Sources.CONTEXT;
					process((NodeSet)input, context, builder);

				} else if (Keywords.USE_GLOBAL_CONTEXT.keyword().equals(name)) {
					// process use:repository
					// process children use global context (repository) as source of instances
					source = Sources.GLOBAL_CONTEXT;
					process((NodeSet)input, context, builder);
					
				}

			} else {
				// process element()
				builder.startElement(new QName(name, ns), null);
				process((NodeSet)input, context, builder);
				builder.endElement();
			}

		}

		default:
			// process node()
			builder.addReferenceNode(input);
			
		}
		
	}
	
	private void processReference(NodeProxy input, NodeSet context, MemTreeBuilder builder) throws XPathException {
		AnimoIndexWorker wk = (AnimoIndexWorker) this.context.getBroker().getIndexController().getWorkerByIndexId(AnimoIndex.ID); 
		NodeProxy res = wk.getNode(input.getNode().getLocalName());
		NodeSet newContext = process(children(input), context);
		process(res, newContext, builder);
	}

}
