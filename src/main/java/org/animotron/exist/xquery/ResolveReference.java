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

import org.animotron.exist.index.AnimoIndex;
import org.animotron.exist.index.AnimoIndexWorker;
import org.animotron.exist.interpreter.AnimoContext;
import org.exist.dom.NodeSet;
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
public class ResolveReference extends BasicFunction {
	
	private static String NAME = "resolve-reference"; 
	
	private static SequenceType ARG1 = 
		new FunctionParameterSequenceType("name", Type.STRING, Cardinality.ONE_OR_MORE, "The name of instance for resolving");
	
	private static SequenceType ARG2 = 
		new FunctionParameterSequenceType("source", Type.NODE, Cardinality.ZERO_OR_MORE, "The source for resolving");
	
	private static FunctionReturnSequenceType RES = 
		new FunctionReturnSequenceType(Type.NODE, Cardinality.ZERO_OR_MORE, "Returns properties nodes");

	public final static FunctionSignature[] signature = {
		new FunctionSignature(
				new QName(NAME, AnimoModule.NAMESPACE_URI, AnimoModule.PREFIX),
				"Resolve reference in global context",
				new SequenceType[] {ARG1}, RES),
		new FunctionSignature(
				new QName(NAME, AnimoModule.NAMESPACE_URI, AnimoModule.PREFIX),
				"Resolve properties in input source",
				new SequenceType[] {ARG1, ARG2}, RES)
	};
	
	public ResolveReference(XQueryContext context, FunctionSignature signature) {
		super(context, signature);
	}

	@Override
	public Sequence eval(Sequence[] args, Sequence contextSequence) throws XPathException {
		SequenceIterator i = args[0].iterate();
		Sequence res = new ValueSequence();
		if (args.length == 1){
			AnimoIndexWorker wk = (AnimoIndexWorker) context.getBroker().getIndexController().getWorkerByIndexId(AnimoIndex.ID);
			while (i.hasNext()){
				String name = i.nextItem().getStringValue();
				NodeSet set = wk.getNode(name);
				res.addAll(set);
			}
		} else {
			AnimoContext source = new AnimoContext(args[1].toNodeSet());
			while (i.hasNext()){
				String name = i.nextItem().getStringValue();
				NodeSet set = source.resolveReference(name);
				res.addAll(set);
			}
		}
		return res;
	}
	
}
