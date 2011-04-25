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

import org.exist.dom.ElementImpl;
import org.exist.dom.NodeImpl;
import org.exist.dom.NodeProxy;
import org.exist.dom.NodeSet;
import org.exist.memtree.MemTreeBuilder;
import org.exist.xquery.XPathException;
import org.exist.xquery.value.Sequence;

/**
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class ProcessReference extends Process {
	
	ProcessReference(Controller controller) {
		super(controller);
	}
	
	private NodeSet resolveReference (){
		if (controller.getSource() == Sources.GLOBAL_CONTEXT) {
			return controller.getIndexWorker().getNode(controller.getCurrentFlow().getLocalName());
		} else {
			//LocationStep step = new LocationStep(context, axis, new NameTest(Type.ELEMENT, new QName (name, ns.namespace())));
			//AnalyzeContextInfo info = new AnalyzeContextInfo(context);
			//info.setFlags(Expression.UNORDERED);
			//PathExpr exp = new PathExpr(context);
			//exp.add(step);
			//exp.analyze(info);
			//exp.reset();
			//res = exp.eval(set);
			//map.put(name, res);
			return null;
		}
		
	}

	public void process (MemTreeBuilder builder) throws XPathException{
		Sequence newContext;
		if (!getCurrentFlow().hasChildNodes()){
			Controller ctrl = new Controller(getXQueryContext(), getCurrentFlow().getChildNodes() , getContext());
			newContext = ctrl.process();
			controller.pushContext(newContext);
		}
		NodeSet input = resolveReference();
		process(input, builder);
	}
	
	private void process(NodeSet input, MemTreeBuilder builder) throws XPathException {
		for (NodeProxy i : input){
			controller.pushFlow((ElementImpl) i.getNode());
			controller.process((NodeImpl) i.getNode(), builder);
		}
	}
	
}
