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

import org.animotron.graph.AnimoGraph;
import org.animotron.graph.GraphOperation;
import org.animotron.graph.RelationshipTypes;
import org.animotron.graph.traverser.It;
import org.animotron.io.Pipe;
import org.animotron.manipulator.OnQuestion;
import org.animotron.manipulator.PFlow;
import org.animotron.manipulator.QCAVector;
import org.animotron.statement.operator.Evaluable;
import org.animotron.statement.operator.Operator;
import org.animotron.statement.operator.REF;
import org.animotron.statement.operator.Utils;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Path;
import org.neo4j.graphdb.Relationship;
import org.neo4j.kernel.impl.util.DebugUtil;

import java.util.LinkedList;
import java.util.List;

import static org.animotron.graph.RelationshipTypes.REV;

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
                        Node def = v.getClosestDefEndNode();
                        process(v.getClosest().getStartNode(), op, np);
                    }
                    return null;
                }
            });
        }

        private void process(Node n, Node op, Node np) {
            It it = new It(n);
            try {
                while (it.hasNext()) {
                    Relationship r = it.next();
                    if (r.getEndNode().equals(op)) {
                        op.createRelationshipTo(np, REV);
                    }
                }
            } finally {
                it.close();
            }
        }

    }

}
