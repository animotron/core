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
import org.animotron.statement.operator.Evaluable;
import org.animotron.statement.operator.Query;
import org.animotron.statement.operator.REF;
import org.animotron.statement.relation.Relation;
import org.neo4j.graphdb.Relationship;

import java.io.IOException;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovskyd@gmail.com">Evgeny Gazdovsky</a>
 * 
 */
public class AnimoResultTraverser extends ResultTraverser {
	
    public static AnimoResultTraverser _ = new AnimoResultTraverser();

    protected AnimoResultTraverser() {}

    @Override
    protected void process(GraphHandler handler, PFlow pf, Statement s, Relationship r, int level, boolean isOne, int pos, boolean isLast) throws IOException {
        if (s != null) {
            if (s instanceof Query || s instanceof Evaluable) {
                result(handler, pf, r, level, isOne);
			//workaround IS and USE
			} else if (s instanceof Relation) {
				handler.start(s, r, level++, isOne, pos, isLast);
				handler.end(s, r, --level, isOne, pos, isLast);
            } else {
                handler.start(s, r, level++, isOne, pos, isLast);
                if (!(s instanceof REF)) {
                    node = r.getEndNode();
                    iterate(handler, pf, new It(node), level);
                }
                handler.end(s, r, --level, isOne, pos, isLast);
            }
        }
    }
}