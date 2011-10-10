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
package org.animotron.statement.value;

import org.animotron.exception.AnimoException;
import org.animotron.statement.AbstractStatement;
import org.animotron.statement.Statement;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;

import java.security.MessageDigest;
import java.util.StringTokenizer;

import static org.animotron.Properties.VALUE;
import static org.animotron.graph.AnimoGraph.*;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public abstract class Value extends AbstractStatement {

	protected Value(String name) { super(name); }

    @Override
	public Relationship build(Node parent, Object reference, boolean ready) throws AnimoException {
        if (reference == null)
            return parent.createRelationshipTo(ready ? getEND() : createNode(), relationshipType());
        Node child;
        byte[] hash = hash(reference).digest();
        if (ready) {
            child = getCache(hash);
            if (child != null) {
                return parent.createRelationshipTo(child, relationshipType());
            }
            child = createNode();
            createCache(child, hash);
        } else {
            child = createNode();
        }
        if (reference instanceof Object[][]) {
            for (Object[] o : (Object[][]) reference) {
                child.setProperty(((Value) o[0]).name(), o[1]);
            }
        } else  if (reference instanceof Object[]) {
            Object[] o = (Object[]) reference;
            child.setProperty(((Value) o[0]).name(), o[1]);
        } else {
            VALUE.set(child, reference);
        }
        return parent.createRelationshipTo(child, relationshipType());
	}

    @Override
    public Object reference(Relationship r) {
        Node n = r.getEndNode();
        if (n.hasProperty(name())) {
            return n.getProperty(name());
        } else  if (VALUE.has(n)) {
            return VALUE.get(n);
        } else {
            return null;
        }
    }

    @Override
    public MessageDigest hash(Object reference) {
        MessageDigest md = md();
        if (reference instanceof Object[][]) {
            for (Object[] o : (Object[][]) reference) {
                md.update(((Statement) o[0]).hash(o[1]).digest());
            }
        } else if (reference instanceof Object[]) {
            Object[] o = (Object[]) reference;
            md.update(((Statement) o[0]).hash(o[1]).digest());
        } else {
            if (reference instanceof String ||
                reference instanceof Number ||
                reference instanceof Boolean)
                md.update(reference.toString().getBytes());
        }
        return md;
    }

    public static Object value(Object o) {
        if (o instanceof String) {
            String s = (String) o;
            try {
                return Integer.valueOf(s);
            } catch (NumberFormatException ei) {
                try {
                    return Long.valueOf(s);
                } catch (NumberFormatException el) {
                    try {
                        return Float.valueOf(s);
                    } catch (NumberFormatException ef) {
                        try {
                            return Double.valueOf(s);
                        } catch (NumberFormatException ed) {
                            if (Boolean.FALSE.toString().equals(s))
                                return Boolean.FALSE;
                            if (Boolean.TRUE.toString().equals(s))
                                return Boolean.TRUE;
                            return s;
                        }
                    }
                }
            }
        }
        return o;
    }

    public static String removeWS(String value) {
        StringBuilder buf = new StringBuilder();
        if (value.length() > 0) {
            StringTokenizer tok = new StringTokenizer(value);
            while (tok.hasMoreTokens()) {
                buf.append(tok.nextToken());
                if (tok.hasMoreTokens()) buf.append(' ');
            }
        }
        return buf.toString();
    }

}