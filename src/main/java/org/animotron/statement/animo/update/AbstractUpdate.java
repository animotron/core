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

import javolution.util.FastSet;
import org.animotron.graph.index.Order;
import org.animotron.io.Pipe;
import org.animotron.manipulator.OnQuestion;
import org.animotron.manipulator.PFlow;
import org.animotron.manipulator.QCAVector;
import org.animotron.statement.operator.Evaluable;
import org.animotron.statement.operator.Operator;
import org.animotron.statement.operator.THE;
import org.animotron.statement.operator.Utils;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.index.IndexHits;

import java.io.IOException;
import java.util.List;
import java.util.Set;

import static org.animotron.graph.AnimoGraph.createNode;
import static org.neo4j.graphdb.Direction.INCOMING;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 */
public abstract class AbstractUpdate extends Operator implements Evaluable {

    private Node root;

    protected AbstractUpdate(String... name) { super(name); }

    protected abstract void execute(Set<Relationship> the, Relationship destination, Set<Relationship> target) throws IOException;

    public OnQuestion onCalcQuestion() {
        return new Calc();
    }
    
    class Calc extends OnQuestion {
        @Override
        public void act(PFlow pf) throws IOException {
            Pipe destination = Utils.getByREF(pf);

            execute(destination, Order._.context(pf.getOP().getEndNode()));
        }
    }

    private void execute(Pipe destination, IndexHits<Relationship> it) throws IOException {
        try {
            Set<Relationship> param = new FastSet<Relationship>();
            for (Relationship r : it) {
                param.add(r);
            }
            QCAVector v;
            while ((v = destination.take()) != null) {
                getThe(v);
            }
        } finally {
            it.close();
        }
    }

    private void getThe(QCAVector v) throws IOException {
        List<QCAVector> c = v.getContext();
        if (c != null) {
            for (QCAVector i : c) {
                getThe(i);
            }
        } else {
            Node n = v.getClosest().getEndNode(); 
            Relationship r = n.getSingleRelationship(THE._, INCOMING);
            if (r != null) {
                root = createNode();

                copy(root, n);
            }
        }
    }

    protected void copy(Node root, Node node) {

    }

}