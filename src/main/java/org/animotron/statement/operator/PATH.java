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

import org.animotron.graph.traverser.It;
import org.animotron.io.Pipe;
import org.animotron.manipulator.OnQuestion;
import org.animotron.manipulator.PFlow;
import org.animotron.manipulator.QCAVector;
import org.animotron.statement.ml.CDATA;
import org.animotron.statement.ml.COMMENT;
import org.animotron.statement.ml.DTD;
import org.animotron.statement.ml.QNAME;
import org.animotron.statement.value.VALUE;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;

import java.util.LinkedList;
import java.util.List;

/**
 * Operation 'PATH'. Direct reference to 'the' instance.
 * 
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 */
public class PATH extends Operator implements Evaluable {

	public static final PATH _ = new PATH();

	private PATH() { super("//"); }

    public PATH(String... name) { super(name); }

    @Override
	public OnQuestion onCalcQuestion() {
		return new Calc();
    }

    class Calc extends OnQuestion {

        @Override
		public void act(final PFlow pf) throws Throwable {
            byte[] hash = pf.getOpHash();
            if (!Utils.results(pf, hash)) {
                List<Relationship> set = new LinkedList<Relationship>();
                Pipe pipe = Utils.getByREF(pf, pf.getVector());
                QCAVector v;
                while ((v = pipe.take()) != null) {
                    set.add(v.getClosest());
                }
                It it = new It(pf.getOPNode());
                try {
                    int i = 0;
                    while (it.hasNext()) {
                        Relationship r = it.next();
                        if (i > 0 && !r.isType(REF._)) {
                            process(pf, r, false, set);
                        }
                        i++;
                    }
                } finally {
                    it.close();
                }
            }
        }

        private void process(PFlow pf, Relationship r, boolean isLast, List<Relationship> set) {
            if (r.isType(REF._)) {
                if (isLast) {
                    filter(pf, r, true, false, set);
                }
            } else if (r.isType(VALUE._) || r.isType(QNAME._) || r.isType(CDATA._) || r.isType(COMMENT._) || r.isType(DTD._)) {
                filter(pf, r, false, true, set);
            } else {
                set = filter(pf, r, false, false, set);
                Node n = r.getEndNode();
                It it = new It(n);
                try {
                    while (it.hasNext()) {
                        Relationship i = it.next();
                        process(pf, i, !it.hasNext(), set);
                    }
                } finally {
                    it.close();
                }
            }
        }

        private List<Relationship> filter(PFlow pf, Relationship p, boolean isRef, boolean isValue, List<Relationship> set) {
            List<Relationship> zet = new LinkedList<Relationship>();
            for (Relationship r : set) {
                It it = new It(r.getEndNode());
                try {
                    while (it.hasNext()) {
                        Relationship i = it.next();
                        if (i.isType(p.getType())) {
                            if (isRef || isValue) {
                                if (i.getEndNode().equals(p.getEndNode())) {
                                    if (isValue) {
                                        pf.sendAnswer(i);
                                    } else {
                                        pf.sendAnswer(r);
                                    }
                                }
                            } else {
                                zet.add(i);
                            }
                        }
                    }
                } finally {
                    it.close();
                }
            }
            return zet;
        }

    }
	
}
