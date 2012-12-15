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

import org.animotron.io.Pipe;
import org.animotron.manipulator.OnQuestion;
import org.animotron.manipulator.PFlow;
import org.animotron.manipulator.QCAVector;
import org.animotron.statement.operator.AN;
import org.animotron.statement.operator.Evaluable;
import org.animotron.statement.operator.Operator;
import org.animotron.statement.operator.Utils;
import org.animotron.statement.operator.VALUE;
import org.neo4j.graphdb.Relationship;

import static org.animotron.statement.operator.VALUE.expression;

/**
 * Return object's NAME.
 *
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class ID extends Operator implements Evaluable {

	public static final ID _ = new ID();

	private ID() { super("id"); }

    @Override
    public OnQuestion onCalcQuestion() {
        return new Calc();
    }
    
    class Calc extends OnQuestion {
    	@Override
        public void act(final PFlow pf) {
    		System.out.println("ID "+pf.getVector());

    		Pipe p = AN.getREFs(pf, pf.getVector());
			QCAVector v = null;
			while ((v = p.take()) != null) {
        		System.out.println(v);
        		Relationship r = v.getClosest();
        		if (r.isType(VALUE._)) {
	        		pf.sendAnswer(pf.getVector().answered(r));
        			
        		} else {
	        		String name = Utils.name(v.getClosest().getEndNode());
	        		System.out.println(name);
	        		if (name != null && !name.isEmpty()) {
		        		pf.sendAnswer(pf.getVector().answered(
		                    expression(name)
		                ));
	        		}
        		}
            }
        }
    }
}