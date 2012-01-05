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
package org.animotron.statement.instruction;

import static org.animotron.graph.RelationshipTypes.RESULT;
import static org.neo4j.graphdb.Direction.INCOMING;

import org.animotron.graph.Properties;
import org.animotron.graph.AnimoGraph;
import org.animotron.graph.GraphOperation;
import org.animotron.manipulator.PFlow;
import org.animotron.statement.AbstractStatement;
import org.animotron.statement.operator.AN;
import org.animotron.statement.operator.Shift;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public abstract class Instruction extends AbstractStatement implements Shift {

    public Instruction(String... name) {
        super(name);
    }
    
    protected void answered(final PFlow pf, final Relationship r) {
		Relationship res = AnimoGraph.execute(new GraphOperation<Relationship>() {
			@Override
			public Relationship execute() {
				Node sNode = pf.getOP().getStartNode().getSingleRelationship(AN._, INCOMING).getStartNode();
				Relationship res = sNode.createRelationshipTo(r.getEndNode(), RESULT);
				Properties.RID.set(res, r.getId());
				//Properties.CID.set(res, pf.getLastContext().getId());
				return res;
			}
		});

		//XXX: fix context
		pf.sendAnswer(pf.getVector().answered(res));
    }
 
//    @Override
//	public Relationship build(Node parent, Object reference, byte[] hash, boolean ready, boolean ignoreNotFound) throws AnimoException {
//        return AN._.build(parent, name(), hash, ready, ignoreNotFound);
//	}

}