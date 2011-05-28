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
package org.animotron.exist.xquery;

import org.exist.dom.QName;
import org.exist.xquery.BasicFunction;
import org.exist.xquery.Cardinality;
import org.exist.xquery.FunctionSignature;
import org.exist.xquery.XPathException;
import org.exist.xquery.XQueryContext;
import org.exist.xquery.value.FunctionParameterSequenceType;
import org.exist.xquery.value.FunctionReturnSequenceType;
import org.exist.xquery.value.Sequence;
import org.exist.xquery.value.SequenceType;
import org.exist.xquery.value.Type;

/**
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 * 
 */
public class Process extends BasicFunction {
	
	private static String NAME = "process"; 
	
	private static SequenceType ARG1 = 
		new FunctionParameterSequenceType("flow", Type.ITEM, Cardinality.ZERO_OR_MORE, "The processed animo flow");
	
	private static SequenceType ARG2 = 
		new FunctionParameterSequenceType("context", Type.ITEM, Cardinality.ZERO_OR_MORE, "The context");
	
	private static FunctionReturnSequenceType RES = 
		new FunctionReturnSequenceType(Type.ITEM, Cardinality.ZERO_OR_MORE, "Returns processed sequence");

	public final static FunctionSignature[] signature = {
		new FunctionSignature(
			new QName(NAME, AnimoModule.NAMESPACE_URI, AnimoModule.PREFIX),
			"Process flow with context",
			new SequenceType[]{ARG1, ARG2}, RES),
		new FunctionSignature(
			new QName(NAME, AnimoModule.NAMESPACE_URI, AnimoModule.PREFIX),
			"Process flow with empty context",
			new SequenceType[]{ARG1}, RES)
	};
	
	public Process(XQueryContext context, FunctionSignature signature) {
		super(context, signature);
	}

	@Override
	public Sequence eval(Sequence[] args, Sequence contextSequence) throws XPathException {
		//Controller controller = args.length == 1 ? new Controller(context, args[0]) : new Controller(context, args[0], args[1]);
		//Sequence res = controller.process();
		return null;
	}

}
