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
import org.animotron.statement.Statement;
import org.animotron.statement.link.LINK;
import org.animotron.statement.ml.*;
import org.animotron.statement.operator.combinator.EACH;
import org.animotron.statement.operator.combinator.MAP;
import org.animotron.statement.value.AbstractValue;
import org.animotron.statement.value.VALUE;
import org.neo4j.graphdb.Relationship;

import java.io.IOException;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class JExpression extends AbstractExpression {
	
	Object[][] e;
	
    public JExpression(Object[]... e) {
        this(new FastGraphBuilder(), e);
    }

    public JExpression(GraphBuilder builder, Object[]... e) {
        super(builder);
        this.e = e;
    }

    @Override
    public void build() throws Exception {
        for(Object[] i : e) {
            build(i);
        }
    }

    private void build(Object[]... e) throws AnimoException, IOException {
        if (e != null)
            for (Object[] i : e)
                if (i instanceof Object[][]) {
                    build((Object[][]) i);
                } else {
                    buildStatement(i);
                }
    }

    private void buildStatement(Object[] e) throws AnimoException, IOException {
        if (e.length == 1) {
            builder.bind((Relationship) e[0]);
        } else {
            builder.start((Statement) e[0], e[1]);
            if (e[2] instanceof Object[][]) {
                build((Object[][]) e[2]);
            } else if (e[2] != null){
                buildStatement((Object[]) e[2]);
            }
            builder.end();
        }
    }

    public static Object[] _(Statement statement, Object[] p) {
        Object[] e = {statement, null, p};
        return e;
    }

    public static Object[] _(Statement statement, Object[]... p) {
        Object[] e = {statement, null, p};
        return e;
    }

    public static Object[] _(Statement statement, Object reference) {
        Object[] e = {statement, reference, null};
        return e;
    }

    public static Object[] _(Statement statement, Object reference, Object[]... p) {
        Object[] e = {statement, reference, p};
        return e;
    }

    public static Object[] _(Object[]... p) {
        Object[] e = _(LINK._, p);
        return e;
    }

    public static Object[] each(Object[]... p) {
        Object[] e = _(EACH._, p);
        return e;
    }

    public static Object[] map(Object[]... p) {
        Object[] e = _(MAP._, p);
        return e;
    }

    public static Object[] _(Relationship r) {
        Object[] e = {r};
        return e;
    }

    private static Object[] $(AbstractValue s, Object[]... ref) {
        return _(s, ref, null);
    }

	public static Object[] element(String name) {
		return $(ELEMENT._, name(name));
	}

	public static Object[] element(String name, Object[]... p) {
		return _(ELEMENT._, name(name), p);
	}
	
    public static Object[] attribute(String name) {
        return $(ATTRIBUTE._, name(name));
    }

    public static Object[] attribute(String name, Object value) {
        return $(ATTRIBUTE._, name(name), value(value));
    }

    public static Object[] entity(String name) {
        return $(ENTITY._, name(name));
    }

    public static Object[] comment() {
        return $(COMMENT._);
    }

    public static Object[] comment(String value) {
        return _(COMMENT._, value);
    }

    public static Object[] cdata() {
        return _(CDATA._);
    }

    public static Object[] cdata(String value) {
        return _(CDATA._, value);
    }

    public static Object[] pi(String name, String value) {
        return $(PI._, name(name), value(value));
    }

    public static Object[] pi(String value) {
        return $(PI._, value(value));
    }

    public static Object[] dtd() {
        return _(DTD._);
    }

    public static Object[] dtd(String value) {
        return _(DTD._, value);
    }

    public static Object[] namespace(String name, String value) {
        return $(NS._, name(name), value(value));
    }

    public static Object[] namespace(String value) {
        return $(NS._, value(value));
    }

    public static Object[] name(String value) {
        return _(NAME._, value);
    }

    public static Object[] value(Object value) {
        return _(VALUE._, value);
    }

    public static Object[] value(Object value, Object[] p) {
        return _(VALUE._, value, p);
    }

    public static Object[] text(Object value) {
        return _(TEXT._, value);
    }

    public static Object[] text(String value, Object[] p) {
        return _(TEXT._, value, p);
    }

}