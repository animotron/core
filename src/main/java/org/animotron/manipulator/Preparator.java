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
package org.animotron.manipulator;

import org.animotron.graph.RelationshipTypes;
import org.animotron.marker.AbstractMarker;
import org.animotron.marker.Marker;
import org.animotron.statement.Statement;
import org.animotron.statement.operator.Prepare;
import org.animotron.statement.operator.THE;
import org.jetlang.channels.Subscribable;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Path;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.traversal.Evaluation;
import org.neo4j.graphdb.traversal.Evaluator;
import org.neo4j.graphdb.traversal.TraversalDescription;
import org.neo4j.kernel.Traversal;
import org.neo4j.kernel.Uniqueness;

import java.io.IOException;
import java.util.Iterator;

import static org.neo4j.graphdb.traversal.Evaluation.*;

/**
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class Preparator extends StatementManipulator {
	
	public static Preparator _ = new Preparator();
	
	private Preparator() {};
	
	private static TraversalDescription TD =
	     Traversal.description()
		.depthFirst()
		.uniqueness(Uniqueness.RELATIONSHIP_PATH)
        .evaluator(new Evaluator() {
            @Override
            public Evaluation evaluate(Path path) {
                if (path.length() == 2) {
                    if (THE._.THE_NODE().equals(path.endNode()))
                        return INCLUDE_AND_PRUNE;
                    Relationship r = path.relationships().iterator().next();
                    if (RelationshipTypes.REF.equals(r.getType()))
                        return INCLUDE_AND_PRUNE;
                } else if (path.length() < 2) {
                    return EXCLUDE_AND_CONTINUE;
                }
                return EXCLUDE_AND_PRUNE;
            }
        });
	
	
	public void execute(Node op) throws IOException {
		Iterator<Path> it = TD.traverse(op).iterator();
		while (it.hasNext()) {
			super.execute(it.next().relationships().iterator().next());
		}
	}
	
	@Override
	public boolean canGo(Statement statement) {
		return statement instanceof Prepare;
	}

	@Override
	public Subscribable<PFlow> onQuestion(Statement statement, Relationship op) {
		return ((Prepare) statement).onPrepareQuestion();
	}

	@Override
	public Marker marker() {
		return PrepareMarker._;
	}
	
	private static class PrepareMarker extends AbstractMarker {
		
		private static final Marker _ = new PrepareMarker();
		private PrepareMarker() {super(RelationshipTypes.PREPARE);}

		@Override
		public Manipulator manipulator() {
			return Preparator._;
		}
		
	}

}