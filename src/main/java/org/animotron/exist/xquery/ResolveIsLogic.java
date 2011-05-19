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

import org.animotron.graph.AnimoGraph;
import org.exist.dom.QName;
import org.exist.xquery.BasicFunction;
import org.exist.xquery.Cardinality;
import org.exist.xquery.FunctionSignature;
import org.exist.xquery.XPathException;
import org.exist.xquery.XQueryContext;
import org.exist.xquery.value.FunctionParameterSequenceType;
import org.exist.xquery.value.FunctionReturnSequenceType;
import org.exist.xquery.value.Sequence;
import org.exist.xquery.value.SequenceIterator;
import org.exist.xquery.value.SequenceType;
import org.exist.xquery.value.Type;
import org.exist.xquery.value.ValueSequence;

/**
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 * 
 */
public class ResolveIsLogic extends BasicFunction {
	
	private static String UP_NAME = "resolve-up-is-logic"; 
	private static String DOWN_NAME = "resolve-down-is-logic"; 
	
	private static SequenceType[] ARG = new SequenceType[] {
		new FunctionParameterSequenceType("name", Type.STRING, Cardinality.ONE_OR_MORE, "The name for resolving")
	};
	
	private static FunctionReturnSequenceType RES = 
		new FunctionReturnSequenceType(Type.NODE, Cardinality.ZERO_OR_MORE, "Returns resolved node set");

	public final static FunctionSignature[] signature = {
		new FunctionSignature(
			new QName(UP_NAME, AnimoModule.NAMESPACE_URI, AnimoModule.PREFIX),
			"Resolve \"is\" logic up.",
			ARG, RES),
		new FunctionSignature(
			new QName(DOWN_NAME, AnimoModule.NAMESPACE_URI, AnimoModule.PREFIX),
			"Resolve \"is\" logic down.",
			ARG, RES)
	};
	
	public ResolveIsLogic(XQueryContext context, FunctionSignature signature) {
		super(context, signature);
	}

	@Override
	public Sequence eval(Sequence[] args, Sequence contextSequence) throws XPathException {
		SequenceIterator i = args[0].iterate();
		Sequence res = new ValueSequence();
		if (isCalledAs(UP_NAME)){
			while (i.hasNext()){
				res.addAll(AnimoGraph.resolveUpIsLogic(i.nextItem().getStringValue()));
			}
		} else {
			while (i.hasNext()){
				res.addAll(AnimoGraph.resolveDownIsLogic(i.nextItem().getStringValue()));
			}
		}
		return res;
	}
	
}
