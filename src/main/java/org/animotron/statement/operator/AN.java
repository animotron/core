/*
 *  Copyright (C) 2011-2013 The Animo Project
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

import org.animotron.io.Pipe;
import org.animotron.manipulator.OnQuestion;
import org.animotron.manipulator.PFlow;
import org.animotron.manipulator.QCAVector;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Path;
import org.neo4j.graphdb.Relationship;

/**
 * Operation 'AN'. Direct reference to 'the' instance.
 * 
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 */
public class AN extends Operator implements Reference, Evaluable, Shift {
	
	public static final AN _ = new AN();
	
	private static boolean debug = false;
	
	private AN() { super("an"); }

    public AN(String... name) { super(name); }

    @Override
	public OnQuestion onCalcQuestion() {
		return new Calc();
    }
    
    class Calc extends OnQuestion {
	
		@Override
		public void act(final PFlow pf) throws Throwable {
			byte[] hash = pf.getOpHash();

			if (debug) System.out.println("AN "+Thread.currentThread());
			//System.out.println("AN "+pf.getVector());
			//pf.sendAnswer(new QCAVector(op,op));
			
			if (!Utils.results(pf, hash)) {
				Pipe pipe = getREFs(pf, pf.getVector());
				QCAVector r;
				while ((r = pipe.take()) != null) {
					System.out.println("AN "+r);
					if (r.getQuestion().equals(pf.getVector().getQuestion()))
						pf.sendAnswer(r);
//						pf.sendAnswer(DEF._.get(r.getAnswer().getEndNode()), r);
					
					else 
//						pf.sendAnswer(DEF._.get(r.getAnswer().getEndNode()), r);
						pf.sendAnswer(new QCAVector(pf.getOP(), r, r.getUnrelaxedAnswer()));
						
				}
			}
		}
    }
	
	public static Pipe getREFs(final PFlow pf, final QCAVector vector) {
		return Utils.getByREF(pf, vector);
	}
	
	public static Relationship endOfHasA(Path path) {
		Node node = path.startNode();
		for (Relationship rr : path.relationships()) {
			if (!(rr.isType(AN._) && node.equals(rr.getEndNode()))) {
				return rr;
			}
			node = rr.getEndNode();
		}
		return null;
	}

	public static boolean beginWithHasA(Path path) {
		Node node = path.startNode();
		for (Relationship rr : path.relationships()) {
			if (!(rr.isType(AN._) && node.equals(rr.getEndNode()))) {
				return false;
			}
			return true;
		}
		return false;
	}
}
