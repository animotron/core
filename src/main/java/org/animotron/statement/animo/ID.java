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
package org.animotron.statement.animo;

import org.animotron.expression.JExpression;
import org.animotron.manipulator.OnQuestion;
import org.animotron.manipulator.PFlow;
import org.animotron.manipulator.QCAVector;
import org.animotron.statement.operator.Evaluable;
import org.animotron.statement.operator.Operator;
import org.animotron.statement.operator.Utils;
import org.jetlang.channels.Subscribable;

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
    public Subscribable<PFlow> onCalcQuestion() {
        return question;
    }

    private OnQuestion question = new OnQuestion(){
        @Override
        public void onMessage(final PFlow pf) {
           for (QCAVector v : Utils.getByREF(pf)) {
                try {
                    pf.sendAnswer(pf.getVector().answered(
	                        new JExpression(
	                            value(
	                                Utils.name(v.getClosest().getEndNode())
	                            )
	                        )
                        )
                    );
                } catch (Exception e) {
                    pf.sendException(e);
                    return;
                }
            }
            pf.done();
        }
    };

}
