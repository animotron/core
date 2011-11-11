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

import org.animotron.Properties;
import org.animotron.expression.JExpression;
import org.animotron.graph.AnimoGraph;
import org.animotron.graph.GraphOperation;
import org.animotron.graph.index.Order;
import org.animotron.manipulator.OnQuestion;
import org.animotron.manipulator.PFlow;
import org.animotron.statement.operator.AN;
import org.animotron.statement.operator.Evaluable;
import org.jetlang.channels.Subscribable;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.index.IndexHits;

import java.io.IOException;

import static org.animotron.expression.JExpression.value;
import static org.animotron.graph.RelationshipTypes.RESULT;
import static org.neo4j.graphdb.Direction.INCOMING;

/**
 *
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 */
public abstract class BibaryMathOperator extends AbstractMathOperator implements Evaluable{

	protected BibaryMathOperator(String name) { super(name); }

    @Override
    public Subscribable<PFlow> onCalcQuestion() {
        return question;
    }

    protected abstract double execute (double a, double b);

    private OnQuestion question = new OnQuestion() {

        @Override
        public void onMessage(final PFlow pf) {
            IndexHits<Relationship> params = Order.queryDown(pf.getOP().getStartNode());
            try {
                double x;
                if (params.hasNext()) {
                    x = param(pf, params.next());
                    while (params.hasNext()) {
                        x = execute(x, param(pf, params.next()));
                    }
                    Relationship r = new JExpression(value(x));
                    final Node rNode = r.getEndNode();
                    final long rID = r.getId();
                    r = AnimoGraph.execute(new GraphOperation<Relationship>() {
                        @Override
                        public Relationship execute() {
                            Node sNode = pf.getOP().getStartNode().getSingleRelationship(AN._, INCOMING).getStartNode();
                            Relationship res = sNode.createRelationshipTo(rNode, RESULT);
                            Properties.RID.set(res, rID);
                            Properties.CID.set(res, pf.getLastContext().getId());
                            return res;
                        }
                    });
                    pf.sendAnswer(r);
                }
            } catch (IOException e) {
                pf.sendException(e);
                return;
            } finally {
                params.close();
            }
            pf.done();
        }

    };

}
