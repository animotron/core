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
package org.animotron.expression;

import org.animotron.exception.AnimoException;
import org.animotron.graph.builder.FastGraphBuilder;
import org.animotron.graph.builder.GraphBuilder;
import org.animotron.statement.LINK;
import org.animotron.statement.Statement;
import org.animotron.statement.ml.*;
import org.animotron.statement.ml.VALUE;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class JExpression extends Expression {
	
	Object[][] e;
	
    public JExpression(Object[]... e) throws Exception {
        this(new FastGraphBuilder(), e);
    }

    public JExpression(GraphBuilder builder, Object[]... e) throws Exception {
        super(builder);
        this.e = e;
        builder.build(this);
    }

    @Override
    public void build() throws AnimoException {
        builder.startGraph();
        for(Object[] i : e) {
            buildExpression(i);
        }
        builder.endGraph();
    }

    private void buildExpression(Object[]... e) throws AnimoException {
        if (e != null)
            for (Object i : e)
                if (i instanceof Object[][]) {
                    buildExpression((Object[][]) i);
                } else {
                    buildStatement((Object[]) i);
                }
    }

    private void buildStatement(Object[] e) throws AnimoException {
        builder.start((Statement) e[0], (String) e[1]);
        buildExpression((Object[][]) e[2]);
        builder.end();
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

    public static Object[] _(Object[]... p) {
        Object[] e = _(LINK._, p);
        return e;
    }

	public static Object[] element(String name) {
		return _(ELEMENT._, name(name));
	}

	public static Object[] element(String name, Object[]... p) {
		return _(ELEMENT._, name(name), p);
	}
	
    public static Object[] attribute(String name) {
        return _(ATTRIBUTE._, name(name));
    }

    public static Object[] attribute(String name, String value) {
        return _(ATTRIBUTE._, name(name), string(value));
    }

    public static Object[] entity(String name, String value) {
        return _(ENTITY._, name(name), string(value));
    }

	public static Object[] comment(String value) {
        return _(COMMENT._, text(value));
	}

    public static Object[] cdata(String value) {
        return _(CDATA._, text(value));
    }

    public static Object[] pi(String name, String value) {
        return _(PI._, name(name), string(value));
    }

    public static Object[] pi(String value) {
        return _(PI._, string(value));
    }

    public static Object[] dtd(String value) {
        return _(DTD._, text(value));
    }

    public static Object[] namespace(String name, String value) {
        return _(NS._, name(name), string(value));
    }

    public static Object[] namespace(String value) {
        return _(NS._, string(value));
    }

    public static Object[] name(String value) {
        return _(NAME._, value);
    }

    public static Object[] string(String value) {
        return _(VALUE._, value);
    }

    public static Object[] text(String value) {
        return _(TEXT._, value);
    }

    public static Object[] text(String value, Object[] p) {
        return _(TEXT._, value, p);
    }
    
}