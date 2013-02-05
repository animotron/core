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
package org.animotron.statement.animo;

import org.animotron.graph.index.Order;
import org.animotron.io.Pipe;
import org.animotron.manipulator.Evaluator;
import org.animotron.manipulator.OnQuestion;
import org.animotron.manipulator.PFlow;
import org.animotron.manipulator.QCAVector;
import org.animotron.statement.instruction.DetermInstruction;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.index.IndexHits;

import java.io.IOException;

/**
 * Operation 'ORDERED'.
 * 
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 */
public class ORDERED extends DetermInstruction {
	
	public static final ORDERED _ = new ORDERED();
	
	private static boolean debug = false;
	
	private ORDERED() { super("ordered"); }
	
    @Override
	public OnQuestion onCalcQuestion() {
		return new Calc();
    }
    
    class Calc extends OnQuestion {
		@Override
		public void act(final PFlow pf) {
			if (debug)
				System.out.println("ORDERED "+pf.getOP().getType());
			
			IndexHits<Relationship> hits = Order._.queryDown(pf.getVector().getQuestion().getEndNode());
			try {
				for (Relationship r : hits) {

                    Pipe in = Evaluator._.execute(pf.getController(), new QCAVector(pf.getVector().lastDefId(), r, pf.getVector().getContext()));
                    QCAVector v;
                    while ((v = in.take()) != null) {
    					pf.sendAnswer(v);
                    }

				}
			} catch (IOException e) {
				pf.sendException(e);
				return;
			} finally {
				hits.close();
			}
		}
    }
}
