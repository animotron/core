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
import org.animotron.exist.AnimoSequence;
import org.animotron.exist.index.AnimoIndex;
import org.animotron.exist.index.AnimoIndexWorker;
import org.exist.dom.DocumentImpl;
import org.exist.dom.ElementImpl;
import org.exist.dom.NewArrayNodeSet;
import org.exist.dom.NodeImpl;
import org.exist.dom.NodeProxy;
import org.exist.dom.NodeSet;
import org.exist.dom.QName;
import org.exist.memtree.MemTreeBuilder;
import org.exist.xquery.XPathException;
import org.exist.xquery.XQueryContext;
import org.exist.xquery.value.Item;
import org.exist.xquery.value.Sequence;
import org.exist.xquery.value.SequenceIterator;
import org.exist.xquery.value.Type;
import org.exist.xquery.value.ValueSequence;
import org.w3c.dom.NodeList;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class Controller {

	private XQueryContext queryContext;
	
	private Sequence flow;
	private Sequence context;
	
	private NodeSet use = new NewArrayNodeSet();
	private AnimoSequence contextStack = new AnimoSequence();
	private Sequence flowStack = new ValueSequence();
	private Sources source = Sources.GLOBAL_CONTEXT;
	
	private ElementImpl currentFlow = null;
	
	private ProcessReference reference = new ProcessReference(this);
	
	public Controller(XQueryContext queryContext, Sequence flow) throws XPathException {
		this(queryContext, flow, Sequence.EMPTY_SEQUENCE);
	}
	
	public Controller(XQueryContext queryContext, NodeList flow, Sequence context) throws XPathException {
		this.queryContext = queryContext;
		this.context = context;
		this.flow = new ValueSequence();
		for (int i = 0; i < flow.getLength(); i++){
			this.flow.add((Item) flow.item(i));
		}
	}

	public Controller(XQueryContext queryContext, Sequence flow, Sequence context) throws XPathException {
		this.queryContext = queryContext;
		this.contextStack.addAll(context);
		this.context = context;
		this.flow = flow;
	}
	
	public Sequence getFlow(){
		return flow;
	}
	
	public ElementImpl getCurrentFlow(){
		return currentFlow;
	}
	
	public Sequence getContext(){
		return context;
	}
	
	public XQueryContext getXQueryContext(){
		return queryContext;
	}
	
	public void pushFlow(ElementImpl input) throws XPathException{
		if (currentFlow != null)
			flowStack.add((Item) currentFlow);
		currentFlow = input;
	}
	
	public void pushContext(Sequence context) throws XPathException{
		contextStack.addAll(context);
		this.context = context;
	}
	
	public Sequence getFlowStack(){
		return flowStack;
	}
	
	public Sequence getContextStack(){
		return contextStack;
	}
	
	public Sources getSource(){
		return source;
	}
	
	public AnimoIndexWorker getIndexWorker() {
        return (AnimoIndexWorker) queryContext.getBroker().getIndexController().getWorkerByIndexId(AnimoIndex.ID);
	}
	
	protected void addUse(NodeProxy node) {
		use.add(node);
	}

	public NodeSet getUse(NodeSet set) {
		return use.intersection(set);
	}
	
	//TODO: write tests
	public NodeSet preferedUse(NodeSet is) {
		if (is.isEmpty())
			return NodeSet.EMPTY_SET;
		
		NodeSet result = new NewArrayNodeSet();
		
		NodeSet use = getUse(is);
		for (NodeProxy node : use) {
			NodeSet useIS = getIndexWorker().resolveDownIsLogic(node);

			if (useIS.isEmpty())
				continue;
			
			NodeSet res = use.intersection(useIS);
			
			if (res.isEmpty())
				result.add(node);
			else
				result.addAll(res);
		}
		
		return result;
	}
	
	public Sequence process() throws XPathException {
		Sequence res = new ValueSequence();
		SequenceIterator i = flow.iterate();
		while (i.hasNext()){
			Item item = i.nextItem();
			if (item.getType() == Type.NODE){
				queryContext.pushDocumentContext();
				MemTreeBuilder builder = queryContext.getDocumentBuilder();
				process((NodeImpl) item, builder);
				res.add(builder.getDocument().getNode(1));
				queryContext.popDocumentContext();
			} else {
				res.add(item);
			}
		}
		return res; 
	}
	
	protected void process(NodeImpl input, MemTreeBuilder builder) throws XPathException {
		if (input.getNodeType() == Type.ELEMENT) {
			process ((ElementImpl) input, builder);
		} else {
			builder.addReferenceNode(new NodeProxy((DocumentImpl) input.getDocumentAtExist(), input.getNodeId(), input.getNodeType()));
		}
	}
	
	private void process(ElementImpl input, MemTreeBuilder builder) throws XPathException {

		String ns = input.getNamespaceURI();
		String name = input.getLocalName();

		if (Namespaces.IC.equals(ns)) {
			// skip ic:* 
			// return as is 
			builder.addReferenceNode(new NodeProxy(input));
			
		} else if (Namespaces.AN.equals(ns)) {
			
			if (Keywords.AN_EMPTY.keyword().equals(name)) {
				// process an:empty
				// return nothing
				return;

			} else if (Keywords.AN_CONTENT.keyword().equals(name)) {
				// process an:content
				// process children
				processChild(input, builder);

			} else if (Keywords.AN_SELF.equals(name, ns)) {
				// process an:self
				// return root
				builder.addReferenceNode(input.getDocument().getFirstChildProxy());
				
			} else {
				// process reference an:*
				reference.process(builder);
				
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

		} else if (Namespaces.DO.equals(ns)) {
			
			if (Keywords.DO_SKIP.keyword().equals(name)) {
				// process do:skip
				// return children
				//for (NodeProxy i : input){
				//	builder.addReferenceNode(i);
				//}

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
				processChild(input, builder);

			} else if (Keywords.USE_CONTEXT_STACK.keyword().equals(name)) {
				// process use:stack
				// process children use context stack as source of instances
				source = Sources.CONTEXT_STACK;
				processChild(input, builder);

			} else if (Keywords.USE_LOCAL_CONTEXT.keyword().equals(name)) {
				// process use:context
				// process children use local context as source of instances
				source = Sources.LOCAL_CONTEXT;
				processChild(input, builder);

			} else if (Keywords.USE_CONTEXT.keyword().equals(name)) {
				// process use:CONTEXT
				// process children use local context and context source as source of instances
				source = Sources.CONTEXT;
				processChild(input, builder);

			} else if (Keywords.USE_GLOBAL_CONTEXT.keyword().equals(name)) {
				// process use:repository
				// process children use global context (repository) as source of instances
				source = Sources.GLOBAL_CONTEXT;
				processChild(input, builder);
				
			}

		} else {
			// process element()
			builder.startElement(new QName(name, ns), null);
			processChild(input, builder);
			builder.endElement();
		}

	}
	
	private void processChild(ElementImpl input, MemTreeBuilder builder) throws XPathException {
		NodeList list = input.getChildNodes(); 
		for (int i = 0; i < list.getLength(); i++){
			process((NodeImpl) list.item(i) , builder);			
		}
	}
	
}
