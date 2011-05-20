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
package org.animotron.interpreter;

import javax.xml.transform.Templates;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerFactoryConfigurationError;
import javax.xml.transform.sax.SAXResult;
import javax.xml.transform.sax.SAXTransformerFactory;
import javax.xml.transform.sax.TemplatesHandler;
import javax.xml.transform.sax.TransformerHandler;

import org.exist.dom.NodeHandle;
import org.exist.dom.NodeProxy;
import org.exist.memtree.DocumentBuilderReceiver;
import org.exist.memtree.MemTreeBuilder;
import org.exist.memtree.NodeImpl;
import org.exist.storage.serializers.Serializer;
import org.exist.util.serializer.Receiver;
import org.exist.util.serializer.ReceiverToSAX;
import org.exist.xquery.XPathException;
import org.exist.xquery.XQueryContext;
import org.exist.xquery.value.NodeValue;
import org.exist.xquery.value.Sequence;
import org.exist.xquery.value.ValueSequence;
import org.exist.xslt.TransformerFactoryAllocator;
import org.w3c.dom.Node;
import org.xml.sax.SAXException;

/**
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
@Deprecated //replaced by calculator
public class ProcessXSLT extends Process {
	
	ProcessXSLT(Controller controller) {
		super(controller);
	}
	
	@Override
	public Sequence eval() throws XPathException {
		
        if (!getCurrentStep().hasChildNodes())
        	return Sequence.EMPTY_SEQUENCE;
        
		XQueryContext context = this.getXQueryContext();
		
		Sequence inputNode = getContext();
		
		Node node = getCurrentStep().getFirstChild();
        TransformerHandler handler = createHandler(node instanceof NodeImpl ? (NodeValue) node : new NodeProxy((NodeHandle) node));

    	Transformer transformer = handler.getTransformer();
    	
    	if (transformer.getClass().getName().equals("org.exist.xslt.TransformerImpl")) {
    		context.pushDocumentContext();
    		Sequence seq = ((org.exist.xslt.Transformer)transformer).transform(inputNode);
    		context.popDocumentContext();
    		return seq;
    	} else {
            ValueSequence seq = new ValueSequence();
    		context.pushDocumentContext();
    		MemTreeBuilder builder = context.getDocumentBuilder();
    		DocumentBuilderReceiver builderReceiver = new DocumentBuilderReceiver(builder, true);
    		SAXResult result = new SAXResult(builderReceiver);
    		result.setLexicalHandler(builderReceiver);
    		handler.setResult(result);
            Receiver receiver = new ReceiverToSAX(handler);
            Serializer serializer = context.getBroker().getSerializer();
            serializer.reset();
            try {
                serializer.setReceiver(receiver, true);
    			serializer.toSAX(inputNode, 1, inputNode.getItemCount(), false, false);
    		} catch (Exception e) {
    			throw new XPathException("Exception while transforming node: " + e.getMessage(), e);
    		}
    		Node next = builder.getDocument().getFirstChild();
            while (next != null) {
                seq.add((NodeValue) next);
                next = next.getNextSibling();
            }
    		context.popDocumentContext();
    		return seq;
        }
		
	}
	
    private TransformerHandler createHandler(NodeValue stylesheetNode) throws TransformerFactoryConfigurationError, XPathException {
    	SAXTransformerFactory factory = TransformerFactoryAllocator.getTransformerFactory(getBroker().getBrokerPool());
		TransformerHandler handler;
		try {
			Templates templates = getSource(factory, stylesheetNode);
			handler = factory.newTransformerHandler(templates);
		} catch (TransformerConfigurationException e) {
			throw new XPathException("Unable to set up transformer: " + e.getMessage(), e);
		}
        return handler;
    }

	private Templates getSource(SAXTransformerFactory factory, NodeValue stylesheetRoot) throws XPathException, TransformerConfigurationException {
		TemplatesHandler handler = factory.newTemplatesHandler();
		try {
			handler.startDocument();
			stylesheetRoot.toSAX(getBroker(), handler, null);
			handler.endDocument();
			return handler.getTemplates();
		} catch (SAXException e) {
			throw new XPathException("A SAX exception occurred while compiling the stylesheet: " + e.getMessage(), e);
		}
	}


	
}
