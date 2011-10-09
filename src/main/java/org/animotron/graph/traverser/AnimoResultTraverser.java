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
import org.animotron.manipulator.PFlow;
import org.animotron.statement.Statement;
import org.animotron.statement.Statements;
import org.animotron.statement.ml.NAME;
import org.animotron.statement.operator.Evaluable;
import org.animotron.statement.operator.Query;
import org.animotron.statement.operator.Reference;
import org.animotron.statement.operator.THE;
import org.animotron.statement.relation.HAVE;
import org.animotron.statement.relation.Relation;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.RelationshipType;

import java.io.IOException;

import static org.animotron.Properties.CID;
import static org.animotron.Properties.RID;
import static org.animotron.graph.AnimoGraph.getDb;
import static org.animotron.graph.RelationshipTypes.REF;
import static org.animotron.graph.RelationshipTypes.RESULT;
import static org.neo4j.graphdb.Direction.OUTGOING;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovskyd@gmail.com">Evgeny Gazdovsky</a>
 * 
 */
public class AnimoResultTraverser extends ResultTraverser {
	
    public static AnimoResultTraverser _ = new AnimoResultTraverser();

    protected AnimoResultTraverser() {}

    @Override
    protected void build(GraphHandler handler, PFlow pf, Relationship r, int level, boolean isOne) throws IOException {

        RelationshipType type = r.getType();
        String typeName = type.name();

        int addedContexts = 0;
        try {
	        Relationship context = getDb().getRelationshipById(
                (Long)r.getProperty(CID.name())
            );
	        pf.addContextPoint(context);
	        addedContexts++;
        } catch (Exception e) {
		}
        
        if (RESULT.name().equals(typeName)) {

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
        
        if (s instanceof Reference || s instanceof HAVE) {
	        pf.addContextPoint(r);
	        addedContexts++;
        }

        if (s != null) {
            if (s instanceof Query || s instanceof Evaluable) {
                result(handler, new PFlow(pf), r, level, isOne);
			//workaround IS and USE
			} else if (s instanceof Relation) {
				handler.start(s, r, level++, isOne);
				handler.end(s, r, --level, isOne);
            } else {
                handler.start(s, r, level++, isOne);
                node = r.getEndNode();
                It it = new It();
                int size = 0;
                if (node.hasRelationship(NAME._.relationshipType(), OUTGOING)) size--;
                try {
                    iterate(handler, pf, it, level, size);
                } finally {
                    it.remove();
                }
                handler.end(s, r, --level, isOne);
            }
        }
        
        while (addedContexts > 0) {
        	pf.popContextPoint();
        	addedContexts--;
        }

    }

}
