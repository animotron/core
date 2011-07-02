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
package org.animotron.operator;

import static org.neo4j.graphdb.Direction.OUTGOING;

import org.animotron.graph.RelationshipTypes;
import org.animotron.manipulator.OnQuestion;
import org.animotron.manipulator.PFlow;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;

/**
 * Operation 'AN'. Direct reference to 'the' instance.
 * 
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 */
public class AN extends AbstarctOperator implements Reference, Evaluable, Cachable {
	
	public static final AN _ = new AN();
	
	private AN() { super("an", "animo/reference"); }
	
	public OnQuestion onCalcQuestion() {
		return question;
	}
	
	private OnQuestion question = new OnQuestion() {

		@Override
		public void onMessage(PFlow pf) {

//			System.out.println("AN THREAD "+Thread.currentThread());

			Relationship op = pf.getOP();
			Node node = op.getEndNode();

			Relationship res = node.getSingleRelationship(
				RelationshipTypes.REF, OUTGOING
			);
			
			System.out.println("AN res = "+res);
			
			pf.sendAnswer(res);
			pf.done();
		}
	};
	
}