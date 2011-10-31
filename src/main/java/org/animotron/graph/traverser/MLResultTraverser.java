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
package org.animotron.graph.traverser;

import org.animotron.graph.handler.GraphHandler;
import org.animotron.graph.serializer.StringResultSerializer;
import org.animotron.manipulator.PFlow;
import org.animotron.statement.Statement;
import org.animotron.statement.ml.*;
import org.animotron.statement.operator.Evaluable;
import org.animotron.statement.operator.Query;
import org.animotron.statement.relation.Relation;
import org.animotron.statement.value.VALUE;
import org.neo4j.graphdb.Relationship;

import java.io.IOException;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovskyd@gmail.com">Evgeny Gazdovsky</a>
 * 
 */
public class MLResultTraverser extends ResultTraverser {

    public static MLResultTraverser _ = new MLResultTraverser();

    protected MLResultTraverser() {}

    @Override
    protected void process(GraphHandler handler, PFlow pf, Statement s, Relationship r, int level, boolean isOne) throws IOException {

        if (s != null) {
            if (s instanceof MLOperator || s instanceof VALUE) {
                if (s instanceof Prefix) {
                    node = r.getEndNode();
                    It it = new It(node);
                    String[] param = {null, null};
                    try {
                        if (it.hasNext()) {
                            int size;
                            Object p = it.next();
                            param[0] = param(pf, p);
                            if (s instanceof ELEMENT) {
                                size = 1;
                            } else {
                                param[1] = param(pf, it);
                                if (param[1] == null) {
                                    if (s instanceof NS) {
                                        if (NAME._.name().equals(p instanceof String ? p : ((Relationship) p).getType().name())) {
                                            param[1] = "";
                                        } else {
                                            param[1] = param[0];
                                            param[0] = "";
                                        }
                                    }
                                    size = 1;
                                } else {
                                    size = 2;
                                }
                            }
                            handler.start(s, param, level++, isOne);
                            iterate(handler, pf, it, level, size);
                            handler.end(s, param, --level, isOne);
                        }
                    } finally {
                        it.remove();
                    }
                } else if (!(s instanceof VALUE) || (s instanceof VALUE && level > 0)) {
                    String param = StringResultSerializer.serialize(pf, r);
                    handler.start(s, param, level++, isOne);
                    handler.end(s, param, --level, isOne);
                }
            } else if (s instanceof Query || s instanceof Evaluable) {
                result(handler, pf, r, level, isOne);
			//workaround IS and USE
			} else if (!(s instanceof Relation)) {
                node = r.getEndNode();
                iterate(handler, pf, new It(node), level, 0);
            }
        }
    }

    private String param(PFlow pf, It it) throws IOException {
        if (it.hasNext()) {
            return param(pf, it.next());
        }
        return null;
    }

    private String param(PFlow pf, Object o) throws IOException {
        return
            o instanceof Relationship
                ? StringResultSerializer.serialize(pf, (Relationship) o)
                : (String) node.getProperty((String) o);
    }

}
