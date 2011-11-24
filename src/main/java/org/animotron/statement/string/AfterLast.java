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
package org.animotron.statement.string;

import org.animotron.Properties;
import org.animotron.expression.JExpression;
import org.animotron.graph.AnimoGraph;
import org.animotron.graph.GraphOperation;
import org.animotron.graph.index.Order;
import org.animotron.graph.serializer.StringResultSerializer;
import org.animotron.manipulator.OnQuestion;
import org.animotron.manipulator.PFlow;
import org.animotron.statement.instruction.Instruction;
import org.animotron.statement.operator.AN;
import org.animotron.statement.operator.Evaluable;
import org.jetlang.channels.Subscribable;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;

import java.io.IOException;

import static org.animotron.expression.JExpression.text;
import static org.animotron.graph.RelationshipTypes.RESULT;
import static org.neo4j.graphdb.Direction.INCOMING;

/**
 * VALUE instruction 'after-last'.
 *
 * Return last chars from input string after last-found defined pattern.
 *
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 */
public class AfterLast extends Instruction implements Evaluable {

	public static final AfterLast _ = new AfterLast();

	private AfterLast() { super("after-last"); }


	@Override
	public Subscribable<PFlow> onCalcQuestion() {
		return question;
	}

    private OnQuestion question = new OnQuestion(){
        @Override
        public void onMessage(final PFlow pf) {

            //UNDERSTAND: if we have more that 2 params, what to do?

        	final Relationship op = pf.getOP();
        	
            Relationship[] params = Order.first(3, pf.getOP().getStartNode());

            //pattern
            String pattern;
			try {
				pattern = StringResultSerializer._.serialize(pf, params[1]);
			} catch (IOException e) {
				pf.sendException(e);
				return;
			}
            String source;
			try {
				source = StringResultSerializer._.serialize(pf, params[2]);
			} catch (IOException e) {
				pf.sendException(e);
				return;
			}
            
            int index = source.lastIndexOf(pattern);
            if (index != -1) {

	            Relationship r;
				try {
					r = new JExpression(
					    text(
					        source.substring(index + 1)
					    )
					);
				} catch (Exception e) {
					pf.sendException(e);
					return;
				}
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

				//XXX: fix context
				pf.sendAnswer(r);
            }

            pf.done();
        }

    };
}
