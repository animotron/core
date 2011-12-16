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

import javolution.util.FastSet;
import org.animotron.graph.index.Order;
import org.animotron.io.PipedInput;
import org.animotron.manipulator.OnQuestion;
import org.animotron.manipulator.PFlow;
import org.animotron.manipulator.QCAVector;
import org.animotron.statement.operator.*;
import org.neo4j.graphdb.Direction;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.index.IndexHits;

import java.io.IOException;
import java.util.List;
import java.util.Set;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 */
public abstract class AbstractUpdate extends Operator implements Evaluable {

	protected AbstractUpdate(String... name) { super(name); }

    protected abstract void execute(Relationship the, Relationship destination, Set<Relationship> target);

    protected abstract void execute(Relationship the, Set<Relationship> target) throws IOException;

    public OnQuestion onCalcQuestion() {
        return question;
    }

    private OnQuestion question = new OnQuestion() {
        @Override
        public void onMessage(PFlow pf) {
            PipedInput<QCAVector> destination = Utils.getByREF(pf);
            try {
                execute(destination, Order.queryDown(pf.getOP().getEndNode()));
            } catch (IOException e) {
                pf.sendException(e);
            }
            pf.done();
        }
    };

    private void execute(PipedInput<QCAVector> destination, IndexHits<Relationship> it) throws IOException {
        try {
            Set<Relationship> target = new FastSet<Relationship>();
            boolean first = true;
            for (Relationship r : it) {
                if (first) {
                    first = false;
                    continue;
                }
                if (r.isType(REF._)) continue;
                target.add(r);
            }
            for (QCAVector v : destination) {
                execute(v, getDestination(v), target);
            }
        } finally {
            it.close();
        }
    }

    private Relationship getDestination(QCAVector v){
        return v.getClosest();
    };

    private void execute(QCAVector v, Relationship destination, Set<Relationship> target) throws IOException {
        List<QCAVector> c = v.getContext();
        if (c != null) {
            for (QCAVector i : c) {
                execute(i, destination, target);
            }
        } else {
            Relationship the = getThe(v.getClosest());
            if (the.getEndNode().equals(destination.getEndNode())) {
                execute(the, target);
            } else {
                execute(the, destination, target);
            }
        }
    }

    private Relationship getThe(Relationship r) {
        return r.getEndNode().getSingleRelationship(THE._, Direction.INCOMING);
    }

}