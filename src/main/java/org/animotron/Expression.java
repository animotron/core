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
 *  GNU Lesser General Public License for more detailstatement.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */
package org.animotron;

import org.animotron.graph.GraphBuilder;
import org.animotron.instruction.Instruction;
import org.animotron.instruction.ml.ATTRIBUTE;
import org.animotron.instruction.ml.CDATA;
import org.animotron.instruction.ml.COMMENT;
import org.animotron.instruction.ml.ELEMENT;
import org.animotron.instruction.ml.TEXT;
import org.animotron.operator.Operator;
import org.neo4j.graphdb.Relationship;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class Expression extends GraphBuilder {
	
	Object[][] e;
	
	public Expression(Object[]... e) {
		this.e = e;
	}
	
	public Relationship build() {
		startGraph();
		for(Object[] i : e) {
			buildExpression(i);
		}
		endGraph();
		return getRelationship();
	}
	
	private void buildExpression(Object[]... e) {
		if (e != null)
			for (Object i : e) 
				buildStatement((Object[]) i);
	}
	
	private void buildStatement(Object[] e) {
		start((Statement) e[0], (String) e[1], (String) e[2], (String) e[3], (String) e[4]);
		buildExpression((Object[][]) e[5]);
		end();
	}
	
	public static Object[] s(Operator statement, String name) {
		Object[] e = {statement, statement.name(), statement.namespace(), name, null, null};
		return e;
	}

	public static Object[] s(Operator statement, String name, Object[]... p) {
		Object[] e = {statement, statement.name(), statement.namespace(), name, null, p};
		return e;
	}

	public static Object[] s(Instruction statement) {
		Object[] e = {statement, statement.prefix(), statement.namespace(), statement.name(), null, null};
		return e;
	}

	public static Object[] s(Instruction statement, Object[]... p) {
		Object[] e = {statement, statement.prefix(), statement.namespace(), statement.name(), null, p};
		return e;
	}

	public static Object[] element(String name) {
		String[] qname = qname(name);
		Object[] e = {ELEMENT.getInstance(), qname[0], null, qname[1], null, null};
		return e;
	}

	public static Object[] element(String name, String ns) {
		String[] qname = qname(name);
		Object[] e = {ELEMENT.getInstance(), qname[0], ns, qname[1], null, null};
		return e;
	}

	public static Object[] element(String name, Object[]... p) {
		String[] qname = qname(name);
		Object[] e = {ELEMENT.getInstance(), qname[0], null, qname[1], null, p};
		return e;
	}
	
	public static Object[] element(String name, String ns, Object[]... p) {
		String[] qname = qname(name);
		Object[] e = {ELEMENT.getInstance(), qname[0], ns, qname[1], null, p};
		return e;
	}
	
	public static Object[] attribute(String name, String value) {
		String[] qname = qname(name);
		Object[] e = {ATTRIBUTE.getInstance(), qname[0], null, qname[1], value, null};
		return e;
	}

	public static Object[] attribute(String name, String ns, String value) {
		String[] qname = qname(name);
		Object[] e = {ATTRIBUTE.getInstance(), qname[0], ns, qname[1], value, null};
		return e;
	}

	public static Object[] text(String value) {
		Object[] e = {TEXT.getInstance(), null, null, null, value, null};
		return e;
	}

	public static Object[] comment(String value) {
		Object[] e = {COMMENT.getInstance(), null, null, null, value, null};
		return e;
	}

	public static Object[] cdata(String value) {
		Object[] e = {CDATA.getInstance(), null, null, null, value, null};
		return e;
	}

	private static String[] qname (String name){
		String[] tmp = {null, name};
		int colon = name.indexOf(":");
		if (colon > 0) {
			tmp[0] = name.substring(0, colon); 
			tmp[1] = name.substring(colon+1); 
		}
		return tmp;
	}


}
