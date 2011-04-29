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
import org.exist.dom.ElementAtExist;
import org.exist.dom.NewArrayNodeSet;
import org.exist.dom.NodeHandle;
import org.exist.dom.NodeProxy;
import org.exist.dom.NodeSet;
import org.exist.dom.QName;
import org.exist.memtree.AttributeImpl;
import org.exist.memtree.CDATASectionImpl;
import org.exist.memtree.CommentImpl;
import org.exist.memtree.ElementImpl;
import org.exist.memtree.MemTreeBuilder;
import org.exist.memtree.NodeImpl;
import org.exist.memtree.ProcessingInstructionImpl;
import org.exist.memtree.TextImpl;
import org.exist.xquery.AnalyzeContextInfo;
import org.exist.xquery.Constants;
import org.exist.xquery.Expression;
import org.exist.xquery.LocationStep;
import org.exist.xquery.PathExpr;
import org.exist.xquery.TypeTest;
import org.exist.xquery.XPathException;
import org.exist.xquery.XQueryContext;
import org.exist.xquery.value.Item;
import org.exist.xquery.value.NodeValue;
import org.exist.xquery.value.Sequence;
import org.exist.xquery.value.SequenceIterator;
import org.exist.xquery.value.Type;
import org.exist.xquery.value.ValueSequence;
import org.w3c.dom.Node;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class Controller {

	private XQueryContext queryContext;
	
	private Sequence flow;
	private Sequence localContext;
	
	private NodeSet use = new NewArrayNodeSet();
	private AnimoSequence context;
	private AnimoSequence contextStack = new AnimoSequence();
	private Sequence flowStack = new ValueSequence();
	private Sequence source = null;
	
	private Item currentFlow = null;
	private Node currentStep = null;
	
	private ProcessReference reference = new ProcessReference(this);
	private ProcessXQuery xquery = new ProcessXQuery(this);
	private ProcessXSLT xslt = new ProcessXSLT(this);
	
	public Controller(XQueryContext queryContext, Sequence flow) throws XPathException {
		this(queryContext, flow, Sequence.EMPTY_SEQUENCE);
	}
	
	public Controller(XQueryContext queryContext, Sequence flow, Sequence context) throws XPathException {
		this.flow = flow;
		this.localContext = context;
		this.context = new AnimoSequence(context);
		this.queryContext = queryContext;
	}
	
	public Node getCurrentStep(){
		return currentStep;
	}
	
	public Sequence getFlow(){
		return flow;
	}
	
	public Item getCurrentFlow(){
		return currentFlow;
	}
	
	public Sequence getFlowStack(){
		return flowStack;
	}
	
	public Sequence getContext(){
		return context;
	}
	
	public Sequence getLocalContext(){
		return localContext;
	}
	
	public Sequence getContextStack(){
		return contextStack;
	}
	
	public XQueryContext getXQueryContext(){
		return queryContext;
	}
	
	public Sequence getSource(){
		return source;
	}
	
	public void pushFlow(Item item) throws XPathException{
		if (currentFlow != null) {
			flowStack.add(item instanceof NodeImpl ? (NodeImpl) currentFlow : new NodeProxy((NodeHandle) item));	
		}
		currentFlow = item;
	}
	
	public void pushContext(Sequence context) throws XPathException{
		contextStack.push(this.localContext);
		this.context.push(context);
		this.localContext = context;
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
		while (i.hasNext()) {
			Item item = i.nextItem();
			if (Type.getSuperType(item.getType()) ==  Type.NODE) {
				queryContext.pushDocumentContext();
				MemTreeBuilder builder = queryContext.getDocumentBuilder();
				process(item, builder);
				Node next = builder.getDocument().getFirstChild();
	            while (next != null) {
	                res.add((NodeValue) next);
	                next = next.getNextSibling();
	            }
				queryContext.popDocumentContext();
			} else {
				res.add(item);
			}
		}
		return res; 
	}
	
	public void process(Item input, MemTreeBuilder builder) throws XPathException {
		if (input.getType() == Type.ELEMENT) {
				if (input instanceof NodeProxy){
					process((ElementAtExist)((NodeProxy) input).getNode(), builder);
				} else {
					process((ElementAtExist) input, builder);
				}
		} else if (input.getType() == Type.NODE) {
    		copy((Node) input, builder);
		} else {
			builder.characters(input.getStringValue());
		}
	}
	
	private void process(ElementAtExist input, MemTreeBuilder builder) throws XPathException {

		currentStep = input;
		
		String ns = input.getNamespaceURI();
		String name = input.getLocalName();

		if (Namespaces.IC.equals(ns)) {
			// skip ic:* 
			// return as is
			copy(input, builder);
			
		} else if (Namespaces.AN.equals(ns)) {
			
			if (Keywords.AN_EMPTY.keyword().equals(name)) {
				// process an:empty
				// return nothing
				return;

			} else if (Keywords.AN_CONTENT.keyword().equals(name)) {
				// process an:content
				// process children
				processChildNodes(input, builder);

			} else if (Keywords.AN_SELF.equals(name, ns)) {
				// process an:self
				// return root
				copy(input.getOwnerDocument().getFirstChild(), builder);
				
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
				// process do:xquery
				// perform in-line XQuery
				xquery.process(builder);

			} else if (Keywords.DO_XSLT.keyword().equals(name)) {
				// process do:xslt
				// perform in-line XSLT
				xslt.process(builder);
				
			}

		} else  if (Keywords.USE_FLOW_STACK.equals(name, ns)) {
			// process use:flow-stack
			// process children use flow stack as source of instances
			source = flowStack;
			processChildNodes(input, builder);

		} else if (Keywords.USE_CONTEXT_STACK.equals(name, ns)) {
			// process use:stack
			// process children use context stack as source of instances
			source = contextStack;
			processChildNodes(input, builder);

		} else if (Keywords.USE_LOCAL_CONTEXT.equals(name, ns)) {
			// process use:context
			// process children use local context as source of instances
			source = localContext;
			processChildNodes(input, builder);

		} else if (Keywords.USE_CONTEXT.equals(name, ns)) {
			// process use:CONTEXT
			// process children use local context and context source as source of instances
			source = context;
			processChildNodes(input, builder);

		} else if (Keywords.USE_GLOBAL_CONTEXT.equals(name, ns)) {
			// process use:repository
			// process children use global context (repository) as source of instances
			source = null;
			processChildNodes(input, builder);
				
		} else {
			// process element()
			builder.startElement(new QName(name, ns), null);
			copyAttributes(input, builder);
			processChildNodes(input, builder);
			builder.endElement();
		}

	}
	
	private void processChildNodes(ElementAtExist input, MemTreeBuilder builder) throws XPathException {
		Node next = input.getFirstChild();
        while (next != null) {
        	if (next.getNodeType() == Type.ELEMENT){
        		process((ElementAtExist) next, builder);
        	} else {
        		copy(next, builder);
        	}
        	next = next.getNextSibling();
        }
	}
	
	private void copyAttributes(ElementAtExist input, MemTreeBuilder builder) throws XPathException {
		SequenceIterator i = getAttributes(input).iterate();
		while (i.hasNext()) {
			Item item = i.nextItem();
			if (item instanceof AttributeImpl) {
				AttributeImpl attr = (AttributeImpl) item;
				builder.addAttribute(new QName(attr.getLocalName(), attr.getNamespaceURI()), attr.getNodeValue());	
			} else {
				builder.addReferenceNode(new NodeProxy((NodeHandle) item));
			}
		}
	}
	
	private Sequence getAttributes(ElementAtExist input) throws XPathException {
		LocationStep step = new LocationStep(queryContext, Constants.CHILD_AXIS, new TypeTest(Type.ATTRIBUTE));
		AnalyzeContextInfo info = new AnalyzeContextInfo(queryContext);
		info.setFlags(Expression.UNORDERED);
		PathExpr exp = new PathExpr(queryContext);
		exp.add(step);
		exp.analyze(info);
		exp.reset();
		ValueSequence seq = new ValueSequence();
		if (input instanceof ElementImpl) {
			seq.add((Item) input);
		} else {
			seq.add(new NodeProxy((NodeHandle) input));
		}
		return exp.eval(seq);
	}
	
	private void copy(Node input, MemTreeBuilder builder) throws XPathException{
		if (input instanceof ElementAtExist) {
			copy((ElementAtExist) input, builder);
		} else {
			if (input instanceof NodeImpl){
				copy ((NodeImpl) input, builder);
			} else {
				builder.addReferenceNode(new NodeProxy((NodeHandle) input));
			}
		}
	}

	private void copyChildNodes(ElementAtExist input, MemTreeBuilder builder) throws XPathException {
		Node next = input.getFirstChild();
		while (next != null) {
			if (next.getNodeType() == Type.ELEMENT) {
				copy((ElementAtExist) next, builder);
			} else {
				copy((NodeImpl) next, builder);
			}
			next = next.getNextSibling();
		}
	}
	
	private void copy(ElementAtExist input, MemTreeBuilder builder) throws XPathException {
		builder.startElement(input.getQName(), null);
		copyAttributes(input, builder);
		copyChildNodes(input, builder);
		builder.endElement();
	}
	
	private void copy(NodeImpl input, MemTreeBuilder builder) throws XPathException {
		
		int type = ((NodeImpl) input).getType();
		
		if (type == Type.TEXT) {
			builder.characters(((TextImpl)input).getNodeValue());
			
		} else if (type == Type.CDATA_SECTION) {
			builder.cdataSection(((CDATASectionImpl)input).getData());
			
		} else if (type == Type.COMMENT) {
			builder.comment(((CommentImpl)input).getData());
			
		} else if (type == Type.PROCESSING_INSTRUCTION) {
			builder.processingInstruction(((ProcessingInstructionImpl)input).getTarget(), ((ProcessingInstructionImpl)input).getData());
			
		}

	}
	
}
