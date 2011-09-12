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
import org.animotron.statement.ml.*;

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
		start((Statement) e[0], (String) e[1]);
		buildExpression((Object[][]) e[2]);
		end();
	}
	
	public static Object[] _(Statement statement, String reference) {
		Object[] e = {statement, reference, null};
		return e;
	}

    public static Object[] _(Statement statement, Object[]... p) {
        Object[] e = {statement, null, p};
        return e;
    }

    public static Object[] _(Statement statement, String reference, Object[]... p) {
        Object[] e = {statement, reference, p};
        return e;
    }

	public static Object[] element(String name) {
		return _(ELEMENT._, text(name));
	}

	public static Object[] element(String name, Object[]... p) {
		return _(ELEMENT._, text(name), p);
	}
	
    public static Object[] attribute(String name) {
        return _(ATTRIBUTE._, text(name));
    }

    public static Object[] attribute(String name, String value) {
        return _(ATTRIBUTE._, text(name), text(value));
    }

	public static Object[] comment(String value) {
        return _(COMMENT._, text(value));
	}

	public static Object[] cdata(String value) {
        return _(CDATA._, text(value));
	}

    public static Object[] text(String value) {
        return _(TEXT._, value);
    }

}