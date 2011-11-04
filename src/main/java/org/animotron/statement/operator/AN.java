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
package org.animotron.statement.operator;

import org.animotron.exception.AnimoException;
import org.animotron.manipulator.OnQuestion;
import org.animotron.manipulator.PFlow;
import org.animotron.statement.link.AbstractLink;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;

import static org.animotron.graph.AnimoGraph.createNode;
import static org.animotron.graph.RelationshipTypes.REF;
import static org.neo4j.graphdb.Direction.OUTGOING;

/**
 * Operation 'AN'. Direct reference to 'the' instance.
 * 
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 */
public class AN extends AbstractLink implements Reference, Evaluable {
	
	public static final AN _ = new AN();
	
	private AN() { super("an"); }
	
    @Override
    protected Node createChild(Object reference, boolean ready, boolean ignoreNotFound) throws AnimoException {
        Node node = createNode();
        if (reference != null) {
            node.createRelationshipTo(reference(reference, ignoreNotFound), REF);
        }
        return node;
    }

    @Override
    public Object reference(Relationship r) {
        Node node = r.getEndNode();
        if (node.hasRelationship(REF, OUTGOING)) {
            return THE._.reference(node.getSingleRelationship(REF, OUTGOING));
        }
        return super.reference(r);
    }

    @Override
	public OnQuestion onCalcQuestion() {
		return question;
	}
	
	private OnQuestion question = new OnQuestion() {

		@Override
		public void onMessage(PFlow pf) {
			
			Relationship op = pf.getOP();
			Node node = op.getEndNode();

			System.out.println("AN "+op+" "+reference(op)+" ");

			Relationship res = node.getSingleRelationship(REF, OUTGOING);
			
			//System.out.println("AN res = "+res);
			
			pf.sendAnswer(res, op);
			pf.done();
		}
	};
	
	public Relationship getREF(Relationship op) {
		return op.getEndNode().getSingleRelationship(REF, OUTGOING);
	}
	
}