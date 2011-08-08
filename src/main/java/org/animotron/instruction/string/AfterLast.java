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
package org.animotron.instruction.string;

import org.animotron.Expression;
import org.animotron.exception.EBuilderTerminated;
import org.animotron.graph.serializer.StringResultSerializer;
import org.animotron.instruction.AbstractInstruction;
import org.animotron.instruction.ml.TEXT;
import org.animotron.manipulator.OnQuestion;
import org.animotron.manipulator.PFlow;
import org.animotron.operator.Evaluable;
import org.jetlang.channels.Subscribable;
import org.neo4j.graphdb.Relationship;

import static org.animotron.Expression.text;
import static org.animotron.graph.AnimoGraph.getORDER;
import static org.neo4j.graphdb.Direction.OUTGOING;

/**
 * String instruction 'after-last'.
 *
 * Return last chars from input string after last-found defined pattern.
 *
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 */
public class AfterLast extends AbstractInstruction implements Evaluable {

	public static final AfterLast _ = new AfterLast();

	private AfterLast() { super("after-last", STRING._); }


	@Override
	public Subscribable<PFlow> onCalcQuestion() {
		return question;
	}

    private OnQuestion question = new OnQuestion(){
        @Override
        public void onMessage(PFlow pf) {

            try {
                //UNDERSTAND: if we have more that 2 params, what to do?

                Relationship[] params = getORDER().first(2, pf.getOPNode());

                //pattern
                String pattern = StringResultSerializer.serialize(pf.getStartOP(), params[0]);
                String source = StringResultSerializer.serialize(pf.getStartOP(), params[1]);
                
                int index = source.lastIndexOf(pattern);
                if (index != -1) {

                	System.out.println("AfterLast = "+source.substring(index + 1));
		            Relationship r = new Expression(
		                text(
		                    source.substring(index + 1)
		                )
		            );
		
		            pf.sendAnswer(r.getEndNode().getSingleRelationship(TEXT._.relationshipType(), OUTGOING));
                }

                pf.done();

            } catch (EBuilderTerminated eBuilderTerminated) {
                eBuilderTerminated.printStackTrace();
            }

        }

    };
}
