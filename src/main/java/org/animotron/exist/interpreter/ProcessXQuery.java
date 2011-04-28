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

import org.exist.security.PermissionDeniedException;
import org.exist.xquery.CompiledXQuery;
import org.exist.xquery.XPathException;
import org.exist.xquery.XQuery;
import org.exist.xquery.XQueryContext;
import org.exist.xquery.value.Sequence;
import org.exist.xquery.value.Type;

/**
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class ProcessXQuery extends Process {
	
	ProcessXQuery(Controller controller) {
		super(controller);
	}
	
	@Override
	public Sequence eval() throws XPathException {
		
		String query = controller.getChildNodes(getCurrentStep()).convertTo(Type.STRING).getStringValue();
		
		XQuery xquery = getBroker().getXQueryService();

		XQueryContext context = getXQueryContext().copyContext();
		
		context.declareVariable("CONTEXT", getContext());
		context.declareVariable("context", getLocalContext());
		context.declareVariable("stack", getContextStack());
		context.declareVariable("flow-stack", getFlowStack());
		
		CompiledXQuery compiled;
		try {
			compiled = xquery.compile(context, query);
			return xquery.execute(compiled, null);
		} catch (PermissionDeniedException e) {
			// TODO Auto-generated catch block
			return Sequence.EMPTY_SEQUENCE; 
		}
		
	}
	
}
