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
import org.animotron.statement.Statements;
import org.animotron.statement.ml.ELEMENT;
import org.animotron.statement.ml.MLOperator;
import org.animotron.statement.ml.Prefix;
import org.animotron.statement.operator.Evaluable;
import org.animotron.statement.operator.Query;
import org.animotron.statement.operator.THE;
import org.animotron.statement.relation.Relation;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.RelationshipType;
import org.neo4j.graphdb.index.IndexHits;

import java.io.IOException;
import java.util.Iterator;

import static org.animotron.Properties.CID;
import static org.animotron.Properties.RID;
import static org.animotron.graph.AnimoGraph.getDb;
import static org.animotron.graph.AnimoGraph.getORDER;
import static org.animotron.graph.RelationshipTypes.REF;
import static org.animotron.graph.RelationshipTypes.RESULT;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovskyd@gmail.com">Evgeny Gazdovsky</a>
 * 
 */
public class MLResultTraverser extends ResultTraverser {

    public static MLResultTraverser _ = new MLResultTraverser();

    protected MLResultTraverser() {}

    @Override
    protected void build(GraphHandler handler, PFlow pf, Relationship r, int level, boolean isOne) throws IOException {

        RelationshipType type = r.getType();
        String typeName = type.name();

        if (RESULT.name().equals(typeName)) {
            Relationship context = getDb().getRelationshipById(
                (Long)r.getProperty(CID.name())
            );
            pf.addContextPoint(context);

            r = getDb().getRelationshipById(
                (Long)r.getProperty(RID.name())
            );

            type = r.getType();
            typeName = type.name();
        }

        Statement s;
        if (REF.equals(r)|| typeName.startsWith(THE._.name())) {
            s = THE._;
        } else {
            s = Statements.relationshipType(typeName);
        }

        if (s != null) {
            if (s instanceof MLOperator) {
                if (s instanceof Prefix) {
                    IndexHits<Relationship> q = getORDER().query(r.getEndNode());
                    Iterator<Relationship> it = q.iterator();
                    String[] param = {null, null};
                    try {
                        param[0] = param(pf, it);
                        int size = q.size();
                        if (s instanceof ELEMENT) {
                            size = size - 1;
                        } else {
                            param[1] = param(pf, it);
                            size = size - 2;
                        }
                        handler.start(s, param, level++, isOne);
                        iterate(handler, pf, it, level, size);
                        handler.end(s, param, --level, isOne);
                    } finally {
                        q.close();
                    }
                } else if (level > 0) {
                    String param = StringResultSerializer.serialize(pf, r);
                    handler.start(s, param, level++, isOne);
                    handler.end(s, param, --level, isOne);
                }
            } else if (s instanceof Query || s instanceof Evaluable) {
                result(handler, pf, r, level, isOne);
			//workaround IS and USE
			} else if (!(s instanceof Relation)) {
                IndexHits<Relationship> q = getORDER().query(r.getEndNode());
                try {
                    iterate(handler, pf, q.iterator(), level, q.size());
                } finally {
                    q.close();
                }
            }
        }

    }

    private String param(PFlow pf, Iterator<Relationship> it) throws IOException {
        if (it.hasNext()) {
            return StringResultSerializer.serialize(pf, it.next());
        }
        return null;
    }

}
