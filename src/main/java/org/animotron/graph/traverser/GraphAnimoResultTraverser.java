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

import org.animotron.Statement;
import org.animotron.Statements;
import org.animotron.graph.GraphHandler;
import org.animotron.graph.RelationshipTypes;
import org.animotron.operator.Evaluable;
import org.animotron.operator.Query;
import org.animotron.operator.THE;
import org.animotron.operator.relation.IS;
import org.animotron.operator.relation.USE;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.RelationshipType;
import org.neo4j.graphdb.index.IndexHits;

import java.io.IOException;

import static org.animotron.Properties.RID;
import static org.animotron.graph.AnimoGraph.getDb;
import static org.animotron.graph.AnimoGraph.getORDER;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovskyd@gmail.com">Evgeny Gazdovsky</a>
 * 
 */
public class GraphAnimoResultTraverser extends GraphResultTraverser{
	
    public static GraphAnimoResultTraverser _ = new GraphAnimoResultTraverser();

    protected GraphAnimoResultTraverser() {}

    @Override
    protected void build(GraphHandler handler, Relationship start_op, Relationship r) throws InterruptedException, IOException {

        RelationshipType type = r.getType();
        String typeName = type.name();

        if (RelationshipTypes.RESULT.name().equals(typeName)) {
            r = getDb().getRelationshipById(
                    (Long)r.getProperty(RID.name())
                );

            type = r.getType();
            typeName = type.name();
        }

        Statement s = Statements.relationshipType(typeName);

        if (RelationshipTypes.REF.name().equals(typeName)
            || typeName.startsWith(THE._.name())) {

            s = THE._;
        }

        if (s != null) {
            if (s instanceof Query || s instanceof Evaluable) {
                result(handler, start_op, r);
			//workaround IS and USE
			} else if (s instanceof IS || s instanceof USE) {
				handler.start(s, r);
				handler.end(s, r);
            } else {
                handler.start(s, r);
                IndexHits<Relationship> q = getORDER().query(r.getEndNode());
                try {
                    for (Relationship i : q) {
                        build(handler, start_op, i);
                    }
                } finally {
                    q.close();
                }
                handler.end(s, r);
            }
        }

    }

}
