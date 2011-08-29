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
 *  GNU Lesser General Public License for more detail statement.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */
package org.animotron;

import org.animotron.exception.AnimoException;
import org.animotron.statement.Statement;
import org.animotron.statement.instruction.ml.*;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class Expression extends AbstractExpression {
	
	Object[][] e;
	
	public Expression(Object[]... e) throws AnimoException {
		this.e = e;
		build();
	}
	
	private void build() throws AnimoException {
		startGraph();
		for(Object[] i : e) {
			buildExpression(i);
		}
		endGraph();
	}
	
	private void buildExpression(Object[]... e) {
		if (e != null)
			for (Object i : e) 
				buildStatement((Object[]) i);
	}
	
	private void buildStatement(Object[] e) {
		start((Statement) e[0], (String) e[1], (String) e[2]);
		buildExpression((Object[][]) e[3]);
		end();
	}
	
	public static Object[] _(Statement statement, String name) {
		Object[] e = {statement, name, null, null};
		return e;
	}

    public static Object[] _(Statement statement, Object[]... p) {
        Object[] e = {statement, null, null, p};
        return e;
    }

    public static Object[] _(Statement statement, String name, Object[]... p) {
        Object[] e = {statement, name, null, p};
        return e;
    }

	public static Object[] element(String name) {
		Object[] e = {ELEMENT._, null, name, null};
		return e;
	}

	public static Object[] element(String name, Object[]... p) {
		Object[] e = {ELEMENT._, null, name, p};
		return e;
	}
	
	public static Object[] attribute(String name, String value) {
		Object[] e = {ATTRIBUTE._, null, name, text(value)};
		return e;
	}

	public static Object[] text(String value) {
		Object[] e = {TEXT._, null, value, null};
		return e;
	}

	public static Object[] comment(String value) {
		Object[] e = {COMMENT._, value, null};
		return e;
	}

	public static Object[] cdata(String value) {
		Object[] e = {CDATA._, null, value, null};
		return e;
	}

}