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

import org.animotron.graph.RelationshipTypes;
import org.animotron.graph.handler.GraphHandler;
import org.animotron.io.PipedInput;
import org.animotron.manipulator.Evaluator;
import org.animotron.manipulator.PFlow;
import org.animotron.statement.Statement;
import org.animotron.statement.Statements;
import org.animotron.statement.operator.Evaluable;
import org.animotron.statement.operator.Query;
import org.animotron.statement.operator.Result;
import org.animotron.statement.operator.THE;
import org.animotron.statement.relation.IS;
import org.animotron.statement.relation.USE;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.RelationshipType;
import org.neo4j.graphdb.index.IndexHits;

import java.io.IOException;

import static org.animotron.Properties.RID;
import static org.animotron.graph.AnimoGraph.getDb;
import static org.animotron.graph.AnimoGraph.getORDER;
import static org.neo4j.graphdb.Direction.OUTGOING;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovskyd@gmail.com">Evgeny Gazdovsky</a>
 * 
 */
public class ResultTraverser extends AnimoTraverser {

    public static ResultTraverser _ = new ResultTraverser();

    protected ResultTraverser() {}

    public void traverse(GraphHandler handler, Relationship start_op, Relationship r) throws IOException {
        handler.startGraph();
        build(handler, new PFlow(Evaluator._, start_op, r), r);
        handler.endGraph();
    }

    @Override
    protected void build(GraphHandler handler, Relationship r) throws IOException {
        build(handler, new PFlow(Evaluator._, r, r), r);
    }

    protected void build(GraphHandler handler, PFlow pf, Relationship r) throws IOException {

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
                result(handler, pf, r);
			} else if (!(s instanceof IS || s instanceof USE)) {
                if (s instanceof Result)
                    handler.start(s, r);
                IndexHits<Relationship> q = getORDER().query(r.getEndNode());
                try {
                    for (Relationship i : q) {
                        build(handler, pf, i);
                    }
                } finally {
                    q.close();
                }
                if (s instanceof Result)
                    handler.end(s, r);
            }
        }

    }

    protected boolean result(GraphHandler handler, PFlow pf, Relationship r) throws IOException {

        boolean found = false;
        Iterable<Relationship> i = r.getEndNode().getRelationships(RelationshipTypes.RESULT, OUTGOING);
        for ( Relationship n : i ) {
            build(
                handler,
                pf,
                getDb().getRelationshipById(
                    (Long)n.getProperty(RID.name())
                )
            );
            found = true;
        }

        if (!found) {
            //UNDERSTAND: calculate current r!
            //System.out.println("READER Execute r = "+r);
            PipedInput in = null;

            in = Evaluator._.execute(pf, r);

            for (Object obj : in) {
                if (obj instanceof Relationship) {
                    build(handler, pf, (Relationship) obj);
                }
            }
        }

        return found;

    }
	
}
