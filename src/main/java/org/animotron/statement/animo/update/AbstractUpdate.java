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
package org.animotron.statement.animo.update;

import org.animotron.graph.index.Order;
import org.animotron.manipulator.Evaluator;
import org.animotron.manipulator.OnQuestion;
import org.animotron.manipulator.PFlow;
import org.animotron.manipulator.QCAVector;
import org.animotron.statement.operator.Evaluable;
import org.animotron.statement.operator.Operator;
import org.animotron.statement.operator.Utils;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.index.IndexHits;

import java.io.IOException;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 */
public abstract class AbstractUpdate extends Operator implements Evaluable {

	protected AbstractUpdate(String name) { super(name); }

    protected abstract void execute(QCAVector destination, IndexHits<Relationship> target);
	
    @Override
	public OnQuestion onCalcQuestion() {
		return question;
	}

	private OnQuestion question = new OnQuestion() {
		@Override
		public void onMessage(final PFlow pf) {
            if (!Utils.results(pf)) {
                IndexHits<Relationship> params = Order.queryDown(pf.getOP().getStartNode());
                try {
                    if (params.hasNext()) {
                        for (QCAVector i : Evaluator._.execute(params.next())) {
                            execute(i, params);
                        }
                    }
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