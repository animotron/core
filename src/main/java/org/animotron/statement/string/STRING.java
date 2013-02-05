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
package org.animotron.statement.string;

import org.animotron.graph.index.Order;
import org.animotron.graph.serializer.Serializer;
import org.animotron.manipulator.OnQuestion;
import org.animotron.manipulator.PFlow;
import org.animotron.statement.instruction.DetermInstruction;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.index.IndexHits;

import java.io.IOException;

import static org.animotron.statement.operator.VALUE.expression;

/**
 * VALUE instruction 'STRING'.
 *
 * Return last chars from input string after last-found defined pattern.
 *
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 */
public class STRING extends DetermInstruction {

	public static final STRING _ = new STRING();

	private STRING() { super("string"); }


	@Override
	public OnQuestion onCalcQuestion() {
		return new Calc();
	}
	
	class Calc extends OnQuestion {
        @Override
        public void act(final PFlow pf) {

            //UNDERSTAND: if we have more that 2 params, what to do?
        	
        	StringBuilder sb = eval(pf, pf.getOP().getStartNode()); 
        	
            if (sb != null && sb.length() > 0) {

	            Relationship r;
				try {
					r = expression(sb.toString());
				} catch (Throwable t) {
					pf.sendException(t);
					return;
				}
				answered(pf, r);
            }
        }
	}
	
	public StringBuilder eval(PFlow pf, Node node) {
        StringBuilder s = new StringBuilder();
        eval(s, pf, node);
        return s;
	}

	public void eval(StringBuilder sb, PFlow pf, Node node) {
        IndexHits<Relationship> hits = Order._.queryDown(node);
        try {
            for (Relationship r : hits) {
                _eval(sb, pf, r);
            }
        } catch (IOException e) {
            pf.sendException(e);
        } finally {
        	hits.close();
        }
	}

    public StringBuilder eval(PFlow pf, Relationship[] hits) {
        StringBuilder s = new StringBuilder();
        eval(s, pf, hits);
        return s;
    }

    public void eval(StringBuilder sb, PFlow pf, Relationship[] hits) {
        try {
            for (Relationship r : hits) {
                _eval(sb, pf, r);
            }
        } catch (IOException e) {
            pf.sendException(e);
        }
    }

    public StringBuilder eval(PFlow pf, Relationship hit) {
        StringBuilder s = new StringBuilder();
        eval(s, pf, hit);
        return s;
    }

    public void eval(StringBuilder sb, PFlow pf, Relationship hit) {
        try {
            _eval(sb, pf, hit);
        } catch (IOException e) {
            pf.sendException(e);
        }
    }

    private void _eval(StringBuilder sb, PFlow pf, Relationship hit) throws IOException {
        sb.append(Serializer.STRING.serialize(pf.getVector().question2(hit)));
    }

}