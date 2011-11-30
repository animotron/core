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
import org.animotron.manipulator.QCAVector;
import org.animotron.statement.Prefix;
import org.animotron.statement.Statement;
import org.animotron.statement.ml.ELEMENT;
import org.animotron.statement.ml.MLOperator;
import org.animotron.statement.ml.NS;
import org.animotron.statement.ml.QNAME;
import org.animotron.statement.operator.Evaluable;
import org.animotron.statement.operator.Query;
import org.animotron.statement.operator.REF;
import org.animotron.statement.relation.USE;
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
    protected void process(GraphHandler handler, PFlow pf, Statement s, QCAVector rr, int level, boolean isOne, int pos, boolean isLast) throws IOException {

        if (s != null) {
            if (s instanceof MLOperator || s instanceof VALUE) {
                if (s instanceof Prefix) {
                    node = rr.getClosest().getEndNode();
                    It it = new It(node);
                    String[] param = {null, null};
                    try {
                        if (it.hasNext()) {
                            Object p = it.next();
                            param[0] = param(pf, p);
                            if (!(s instanceof ELEMENT)) {
                                param[1] = param(pf, it);
                                if (param[1] == null) {
                                    if (s instanceof NS) {
                                        if (QNAME._.name().equals(p instanceof String ? p : ((Relationship) p).getType().name())) {
                                            param[1] = "";
                                        } else {
                                            param[1] = param[0];
                                            param[0] = "";
                                        }
                                    }
                                }
                            }
                            handler.start(s, param, level++, isOne, pos, isLast);
                            iterate(handler, pf, it, level);
                            handler.end(s, param, --level, isOne, pos, isLast);
                        }
                    } finally {
                        it.remove();
                    }
                } else if (!(s instanceof VALUE) || (s instanceof VALUE && level > 0)) {
                    String param = StringResultSerializer._.serialize(pf, rr);
                    handler.start(s, param, level++, isOne, pos, isLast);
                    handler.end(s, param, --level, isOne, pos, isLast);
                }
            } else if (s instanceof Query || s instanceof Evaluable) {
                result(handler, pf, rr, level, isOne);
			//workaround IS and USE
			} else if (!(s instanceof USE || s instanceof REF)) {
                node = rr.getClosest().getEndNode();
                iterate(handler, pf, new It(node), level);
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
                ? StringResultSerializer._.serialize(pf, (Relationship) o)
                : (String) node.getProperty((String) o);
    }

}
