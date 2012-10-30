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
package org.animotron.statement.animo.update;

import javolution.util.FastTable;
import org.animotron.graph.AnimoGraph;
import org.animotron.graph.GraphOperation;
import org.animotron.graph.Properties;
import org.animotron.graph.traverser.It;
import org.animotron.io.Pipe;
import org.animotron.manipulator.OnQuestion;
import org.animotron.manipulator.PFlow;
import org.animotron.manipulator.QCAVector;
import org.animotron.statement.operator.*;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;

import java.util.List;

import static org.animotron.graph.Properties.DEFID;
import static org.animotron.graph.RelationshipTypes.REV;
import static org.neo4j.graphdb.Direction.INCOMING;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 */
public class CHANGE extends Operator implements Evaluable {

    public static final CHANGE _ = new CHANGE();

	private CHANGE() {super("change");}

    @Override
    public OnQuestion onCalcQuestion() {
        return calc;
    }

    private Calc calc = new Calc();

    private class Calc extends OnQuestion {

        @Override
        public void act(final PFlow pf) throws Throwable {
            AnimoGraph.execute(new GraphOperation<Void>() {
                @Override
                public Void execute() throws Throwable {
                    Node op, np;
                    op = np = null;
                    It it = new It(pf.getOPNode());
                    try {
                        int i = 0;
                        while (it.hasNext()) {
                            Relationship r = it.next();
                            if (i > 0 && !r.isType(REF._)) {
                                if (np == null) {
                                    np = r.getEndNode();
                                } else if (op == null) {
                                    op = np;
                                    np = r.getEndNode();
                                    break;
                                }
                            }
                            i++;
                        }
                    } finally {
                        it.close();
                    }
                    Pipe pipe = Utils.getByREF(pf, pf.getVector());
                    QCAVector v;
                    while ((v = pipe.take()) != null) {
                        process(v, op, np);
                    }
                    return null;
                }
            });
        }

        private void process(QCAVector v, Node op, Node np) {
            Node n = v.getClosest().getStartNode();
            It it = new It(n);
            try {
                while (it.hasNext()) {
                    Relationship r = it.next();
                    if (r.getEndNode().equals(op)) {
                        for (Relationship i : def(v)) {
                            Relationship rev = op.createRelationshipTo(np, REV);
                            DEFID.set(rev, i.getId());
                        }
                    }
                }
            } finally {
                it.close();
            }
        }

        private void add (List<Relationship> set, List<Relationship> r) {
            for (Relationship i : r) {
                add(set, i);
            }
        }

        private void add (List<Relationship> set, Relationship r) {
            if (!set.contains(r)) {
                set.add(r);
            }
        }

        private List<Relationship> def (QCAVector v) {
            FastTable<Relationship> set = new FastTable<Relationship>();
            List<QCAVector> context = v.getContext();
            if (context == null) {
                Node def = v.getClosestDefEndNode();
                if (def != null) {
                    add(set, def.getSingleRelationship(DEF._, INCOMING));
                }
            } else {
                for (QCAVector i : context) {
                    add(set, def(i));
                }
            }
            return set;
        }
    }

}
