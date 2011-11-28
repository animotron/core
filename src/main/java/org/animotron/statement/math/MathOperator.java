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
package org.animotron.statement.math;

import org.animotron.expression.JExpression;
import org.animotron.graph.index.Order;
import org.animotron.manipulator.OnQuestion;
import org.animotron.manipulator.PFlow;
import org.animotron.manipulator.QCAVector;
import org.animotron.statement.operator.Evaluable;
import org.animotron.statement.operator.REF;
import org.animotron.statement.operator.Utils;
import org.jetlang.channels.Subscribable;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.index.IndexHits;

import java.io.IOException;

import static org.animotron.expression.JExpression.value;

/**
 *
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 */
public abstract class MathOperator extends AbstractMathOperator implements Evaluable{

	protected MathOperator(String name) { super(name); }

    @Override
    public Subscribable<PFlow> onCalcQuestion() {
        return question;
    }

    protected abstract Number execute (Number a, Number b);
    protected abstract Number execute (Number a);

    private final Number execute (PFlow pf, QCAVector vector) throws IOException {
    	Number a = param(pf, vector);
    	if (a == null) return null;
    	return execute(a);
    }

    private final Number execute (Number a, PFlow pf, QCAVector vector) throws IOException {
    	Number b = param(pf, vector);
    	if (b == null) return a;
    	return execute(a, b);
    }

    private OnQuestion question = new OnQuestion() {

        @Override
        public void onMessage(final PFlow pf) {
        	if (!Utils.results(pf)) {
	            IndexHits<Relationship> params = Order.queryDown(pf.getOP().getStartNode());
	            try {
	                Number x = null;
	                for (Relationship param : params) {
	                	if (param.isType(REF._)) continue;
	                	for (QCAVector r : Utils.getTheRelationships(pf, param)) {
		                	if (x == null) {
		                		if (params.hasNext())
		                			x = param(pf, r);
		                		else
		                			x = execute(pf, r);
		                			
		                	} else
		                		x = execute(x, pf, r);
	                	}
	                }
                    Relationship r = new JExpression(value(x));
                	//Relationship op = pf.getOP().getStartNode().getSingleRelationship(AN._, INCOMING);
                    //XXX: fix context
                    pf.sendAnswer(r);
	            } catch (IOException e) {
	                pf.sendException(e);
	                return;
	            } finally {
	                params.close();
	            }
        	}
            pf.done();
        }

    };

}
