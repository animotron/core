package org.animotron.exist.interpreter;

import java.util.HashMap;
import java.util.Map;

import net.sf.saxon.type.Type;

import org.animotron.Namespaces;
import org.exist.dom.QName;
import org.exist.xquery.AnalyzeContextInfo;
import org.exist.xquery.Constants;
import org.exist.xquery.Expression;
import org.exist.xquery.LocationStep;
import org.exist.xquery.NameTest;
import org.exist.xquery.PathExpr;
import org.exist.xquery.XPathException;
import org.exist.xquery.XQueryContext;
import org.exist.xquery.value.Sequence;

public class AnimoContext {
	
	private Sequence set;
	private XQueryContext context;
	
	private Map <String, Sequence> instances = new HashMap <String, Sequence> ();
	private Map <String, Sequence> properties = new HashMap <String, Sequence> ();
	
	public AnimoContext(Sequence set, XQueryContext context){
		this.context = context;
		this.set = set;
	}
	
	public Sequence getNodes(){
		return set;
	}
	
	private Sequence scan (Map <String, Sequence> map, String name, Namespaces ns, int axis) throws XPathException{
		Sequence res = map.get(name);
		if (res == null){
			LocationStep step = new LocationStep(context, axis, new NameTest(Type.ELEMENT, new QName (name, ns.namespace())));
			AnalyzeContextInfo info = new AnalyzeContextInfo(context);
			info.setFlags(Expression.UNORDERED);
			PathExpr exp = new PathExpr(context);
			exp.add(step);
			exp.analyze(info);
			exp.reset();
			res = exp.eval(set);
			map.put(name, res);
		}
		return res;
	}
	
	public Sequence resolveReference(String name) throws XPathException{
		return scan(instances, name, Namespaces.THE, Constants.SELF_AXIS); 
	}
	
	public Sequence getProperty(String name) throws XPathException{
		return scan(properties, name, Namespaces.HAVE, Constants.DESCENDANT_SELF_AXIS); 
	}
	
}