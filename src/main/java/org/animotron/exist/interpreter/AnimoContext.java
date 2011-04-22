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

/**
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 * 
 */
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