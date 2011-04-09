package org.animotron.exist.xquery.functions;

import org.animotron.exist.index.AnimoIndex;
import org.animotron.exist.index.AnimoIndexWorker;
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

public class ResolveIsLogic extends BasicFunction {
	
	private static String UP_NAME = "resolve-up-is-logic"; 
	private static String DOWN_NAME = "resolve-down-is-logic"; 
	
	private static SequenceType[] ARG = new SequenceType[] {
		new FunctionParameterSequenceType("name", Type.STRING, Cardinality.EXACTLY_ONE, "The name for resolving")
	};
	
	private static FunctionReturnSequenceType RES = new FunctionReturnSequenceType(Type.STRING, Cardinality.ZERO_OR_MORE, "Resolved node set");

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
		String name = args[0].getStringValue();
		AnimoIndexWorker wk = (AnimoIndexWorker) context.getBroker().getIndexController().getWorkerByIndexId(AnimoIndex.ID);
		Sequence res = isCalledAs(UP_NAME) ? wk.resolveUpIsLogic(name) : wk.resolveDownIsLogic(name);
		return res;
	}

}
