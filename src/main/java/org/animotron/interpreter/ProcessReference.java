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

import org.exist.dom.QName;
import org.exist.memtree.MemTreeBuilder;
import org.exist.xquery.AnalyzeContextInfo;
import org.exist.xquery.Constants;
import org.exist.xquery.Expression;
import org.exist.xquery.LocationStep;
import org.exist.xquery.NameTest;
import org.exist.xquery.PathExpr;
import org.exist.xquery.XPathException;
import org.exist.xquery.XQueryContext;
import org.exist.xquery.value.Sequence;
import org.exist.xquery.value.Type;
import org.w3c.dom.Node;

/**
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
@Deprecated //replaced by calculator
public class ProcessReference extends AbstractProcessReference {
	
	ProcessReference(Controller controller) {
		super(controller);
	}
	
	public Sequence eval() throws XPathException{
		
		Sequence source = getSource();
		Node input = getCurrentStep();
		
		if (source == null) {
			return null;//AnimoGraph.getNode(input.getLocalName());
		} else {
			XQueryContext context = getXQueryContext();
			LocationStep step = new LocationStep(context, Constants.SELF_AXIS, new NameTest(Type.ELEMENT, new QName (input.getLocalName(), Namespaces.THE.namespace())));
			AnalyzeContextInfo info = new AnalyzeContextInfo(context);
			info.setFlags(Expression.UNORDERED);
			PathExpr exp = new PathExpr(context);
			exp.add(step);
			exp.analyze(info);
			exp.reset();
			return exp.eval(source);
		}
		
	}

	@Override
	public void process(MemTreeBuilder builder) throws XPathException{
		
		Sequence res = eval();
		
		if (res == null || res.isEmpty())
			return;
		
		if (getCurrentStep().hasChildNodes()){
			Sequence newContext;
			Controller ctrl = new Controller(getXQueryContext(), getChildSteps() , getLocalContext());
			newContext = ctrl.process();
			pushContext(newContext);
		}
		
		process(res, builder);
		
	}
	
}
