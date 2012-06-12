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
package org.animotron.graph.traverser;

import org.animotron.graph.handler.GraphHandler;
import org.animotron.graph.serializer.CachedSerializer;
import org.animotron.manipulator.QCAVector;
import org.animotron.statement.Prefix;
import org.animotron.statement.Statement;
import org.animotron.statement.ml.ELEMENT;
import org.animotron.statement.ml.MLOperator;
import org.animotron.statement.ml.NS;
import org.animotron.statement.ml.QNAME;
import org.animotron.statement.value.VALUE;
import org.neo4j.graphdb.Relationship;

import java.io.IOException;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovskyd@gmail.com">Evgeny Gazdovsky</a>
 * 
 */
public class MLResultTraverser extends AnimoResultTraverser {

    public static MLResultTraverser _ = new MLResultTraverser();

    protected MLResultTraverser() {}

    @Override
    protected void process(GraphHandler handler, Statement s, Statement parent, QCAVector rr, int level, boolean isOne, int pos, boolean isLast) throws IOException {
        if (s instanceof MLOperator || s instanceof VALUE) {
            if (s instanceof Prefix) {
                node = rr.getClosest().getEndNode();
                It it = new It(node);
                String[] param = {null, null};
                try {
                    if (it.hasNext()) {
                        Object p = it.next();
                        param[0] = param(rr, p);
                        if (!(s instanceof ELEMENT)) {
                            param[1] = param(rr, it);
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
                        handler.start(s, parent, param, level++, isOne, pos, isLast);
                        iterate(handler, rr, s, it, level);
                        handler.end(s, parent, param, --level, isOne, pos, isLast);
                    }
                } finally {
                    it.remove();
                }
            } else if (!(s instanceof VALUE) || (s instanceof VALUE)) {
                String param = CachedSerializer.STRING.serialize(rr);
                handler.start(s, parent, param, level++, isOne, pos, isLast);
                handler.end(s, parent, param, --level, isOne, pos, isLast);
            }
        } else {
            super.process(handler, s, parent, rr, level, isOne, pos, isLast);
        }
    }

    private String param(QCAVector rr, It it) throws IOException {
        if (it.hasNext()) {
            return param(rr, it.next());
        }
        return null;
    }

    private String param(QCAVector rr, Object o) throws IOException {
        return
            o instanceof Relationship
                ? CachedSerializer.STRING.serialize(rr.question((Relationship) o))
                : (String) node.getProperty((String) o);
    }

}
