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

import org.animotron.expression.JExpression;
import org.animotron.io.Pipe;
import org.animotron.manipulator.OnQuestion;
import org.animotron.manipulator.PFlow;
import org.animotron.manipulator.QCAVector;
import org.animotron.statement.operator.Evaluable;
import org.animotron.statement.operator.Operator;
import org.animotron.statement.operator.Utils;

import static org.animotron.expression.JExpression.value;

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
        return question;
    }

    private OnQuestion question = new OnQuestion(){
        @Override
        public void act(final PFlow pf) {
        	Pipe p = Utils.getByREF(pf);
        	QCAVector v;
        	while ((v = p.take()) != null) {
        		pf.sendAnswer(pf.getVector().answered(
                    new JExpression(
                        value(
                            Utils.name(v.getClosest().getEndNode())
                        )
                    )
                ));
            }
        }
    };

}
