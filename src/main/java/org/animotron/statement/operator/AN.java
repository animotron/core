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

import org.animotron.exception.AnimoException;
import org.animotron.io.Pipe;
import org.animotron.manipulator.OnQuestion;
import org.animotron.manipulator.PFlow;
import org.animotron.manipulator.QCAVector;
import org.neo4j.graphdb.Node;
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
					if (r.getQuestion().equals(pf.getVector().getQuestion()))
						pf.sendAnswer(r);
					
					else 
						pf.sendAnswer(r.getAnswer(), r);
						//pf.sendAnswer(new QCAVector(pf.getOP(), r, r.getUnrelaxedAnswer()));
						
				}
			}
		}
    }

    @Override
	public Relationship build(Node parent, Object reference, byte[] hash, boolean ready, boolean ignoreNotFound) throws AnimoException {
        Relationship r = super.build(parent, reference, hash, ready, ignoreNotFound);
        r.setProperty(STOPPER._.name(), true);
		return r;
	}

	public Relationship _build(Node parent, Object reference, byte[] hash, boolean ready, boolean ignoreNotFound) throws AnimoException {
        return super.build(parent, reference, hash, ready, ignoreNotFound);
	}

	public static Pipe getREFs(final PFlow pf, final QCAVector vector) {
		return Utils.getByREF(pf, vector);
	}
}
