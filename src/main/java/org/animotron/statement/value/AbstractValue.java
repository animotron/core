/*
 *  Copyright (C) 2011-2012 The Animo Project
 *  http://animotron.org
 *  	
 *  This file is part of Animotron.
 *  
 *  Animotron is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU Affero General Public License as 
 *  published by the Free Software Foundation, either version 3 of 
 *  the License, or (at your option) any later version.
 *  
 *  Animotron is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Affero General Public License for more details.
 *  
 *  You should have received a copy of 
 *  the GNU Affero General Public License along with Animotron.  
 *  If not, see <http://www.gnu.org/licenses/>.
 */
package org.animotron.statement.value;

import javolution.util.FastTable;

import org.animotron.exception.AnimoException;
import org.animotron.statement.AbstractStatement;
import org.animotron.statement.Statement;
import org.animotron.statement.Statements;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;

import java.math.BigDecimal;
import java.util.List;
import java.util.StringTokenizer;

import static org.animotron.graph.Properties.VALUE;
import static org.animotron.graph.AnimoGraph.createNode;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public abstract class AbstractValue extends AbstractStatement {

	protected AbstractValue(String... name) { super(name); }

    @Override
    protected final Node createChild(Object reference, boolean ready, boolean ignoreNotFound) throws AnimoException {
        if (reference == null)
            return createNode();
        Node child = createNode();
        if (reference instanceof List) {
        	for (int i = 0, n = ((List)reference).size(); i < n; i++) {
            	Object[] o = ((List<Object[]>)reference).get(i);
                child.setProperty(((AbstractValue) o[0]).name(), o[1]);
            }
        } else if (reference instanceof Iterable) {
            for (Object[] o : (Iterable<Object[]>) reference) {
                child.setProperty(((AbstractValue) o[0]).name(), o[1]);
            }
        } else if (reference instanceof Object[][]) {
            for (Object[] o : (Object[][]) reference) {
                child.setProperty(((AbstractValue) o[0]).name(), o[1]);
            }
        } else  if (reference instanceof Object[]) {
            Object[] o = (Object[]) reference;
            child.setProperty(((AbstractValue) o[0]).name(), o[1]);
        } else {
            VALUE.set(child, reference);
        }
        return child;
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

    public Object reference(Node n) {
        List<Object[]> ref = new FastTable<Object[]>();
        for (String name : n.getPropertyKeys()) {
            Statement s = Statements.name(name);
            if (s != null) {
                Object[] o = {s, n.getProperty(name)};
                ref.add(o);
            }
        }
        return ref.isEmpty() ? null : ref;
    }

    public static Object value(Object o) {
        if (o instanceof String) {
            String s = (String) o;
            try {
                return Long.valueOf(s);
            } catch (NumberFormatException el) {
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
        return o;
    }

    public static Number number(Object o) {
        if (o instanceof String) {
            String s = (String) o;
            try {
                return Long.valueOf(s);
            } catch (NumberFormatException el) {
                try {
                    return Double.valueOf(s);
                } catch (NumberFormatException ed) {
                    if (Boolean.FALSE.toString().equals(s))
                        return BigDecimal.ZERO;
                    else if (Boolean.TRUE.toString().equals(s))
                        return BigDecimal.ONE;
                    else if (s.isEmpty())
                        return BigDecimal.ZERO;
                    	
                }
            }
        }
        throw new IllegalArgumentException("This is not a number '"+o+"'.");
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