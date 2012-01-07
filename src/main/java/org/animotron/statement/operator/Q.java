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
package org.animotron.statement.operator;

//import static org.animotron.graph.Properties.NAME;
//import static org.neo4j.graphdb.Direction.OUTGOING;
//
//import org.animotron.graph.RelationshipTypes;
//import org.animotron.manipulator.OnQuestion;
//import org.animotron.manipulator.PFlow;
//import org.neo4j.graphdb.Node;
//import org.neo4j.graphdb.Relationship;

/**
 * Operation 'Q'. Computable reference to 'quantity' instance.
 * 
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 */
public class Q extends Operator implements Reference {
	
	public static final Q _ = new Q();
	
	private Q() { super("Q"); }
	
//	public OnQuestion onCalcQuestion() {
//		return question;
//	}
//	
//	private OnQuestion question = new OnQuestion() {
//
//		@Override
//		public void onMessage(PFlow pf) {
//
//			System.out.println("Q "+NAME.get(pf.getOPNode()));
//
//			pf.sendAnswer(pf.getOP());
//			pf.done();
//		}
//	};
}