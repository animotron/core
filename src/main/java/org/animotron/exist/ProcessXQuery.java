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
package org.animotron.exist;


/**
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 */
public class ProcessXQuery {
	
//	ProcessXQuery(Controller controller) {
//		super(controller);
//	}
//	
//	@Override
//	public Sequence eval() throws XPathException {
//		
//        SAXSerializer sax = (SAXSerializer) SerializerPool.getInstance().borrowObject(SAXSerializer.class);
//        try {
//            Serializer serializer = getBroker().getSerializer();
//            serializer.reset();
//            serializer.setProperty(Serializer.GENERATE_DOC_EVENTS, "false");
//            serializer.setSAXHandlers(sax, sax);
//            
//            sax.startDocument();
//            
//    		String query = "";
//            Node next= getCurrentStep().getFirstChild();
//            while(next != null) {
//            	if (next instanceof NodeImpl){
//            		query += serializer.serialize((NodeValue) next);
//            	} else {
//            		query += serializer.serialize(new NodeProxy((NodeHandle) next));
//            	}
//        	    next = next.getNextSibling();
//            }
//            
//            sax.endDocument();
//            
//    		XQuery xquery = getBroker().getXQueryService();
//
//    		XQueryContext context = getXQueryContext().copyContext();
//    		
//    		context.declareVariable("CONTEXT", getContext());
//    		context.declareVariable("context", getLocalContext());
//    		context.declareVariable("stack", getContextStack());
//    		context.declareVariable("flow-stack", getFlowStack());
//    		
//    		CompiledXQuery  compiled = xquery.compile(context, query);
//			return xquery.execute(compiled, null);
//			
//        }
//        catch(SAXException e) {
//            throw new XPathException("A problem occurred while serializing the node set: " + e.getMessage(), e);
//        } catch (PermissionDeniedException e) {
//            throw new XPathException("A problem occurred while serializing the node set: " + e.getMessage(), e);
//		}
//        finally {
//            SerializerPool.getInstance().returnObject(sax);
//        } 
//		
//	}
}
