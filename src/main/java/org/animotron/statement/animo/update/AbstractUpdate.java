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
import org.animotron.graph.AnimoGraph;
import org.animotron.graph.GraphOperation;
import org.animotron.graph.RelationshipTypes;
import org.animotron.graph.index.Order;
import org.animotron.io.Pipe;
import org.animotron.manipulator.DependenciesTracking;
import org.animotron.manipulator.OnQuestion;
import org.animotron.manipulator.PFlow;
import org.animotron.manipulator.QCAVector;
import org.animotron.statement.Statements;
import org.animotron.statement.operator.*;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Path;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.index.IndexHits;
import org.neo4j.graphdb.traversal.Evaluation;
import org.neo4j.graphdb.traversal.Evaluator;
import org.neo4j.kernel.Traversal;
import org.neo4j.kernel.Uniqueness;

import java.util.Iterator;
import java.util.List;
import java.util.Set;

import static org.animotron.graph.AnimoGraph.copy;
import static org.animotron.graph.AnimoGraph.createNode;
import static org.animotron.graph.Properties.ARID;
import static org.animotron.graph.Properties.UUID;
import static org.animotron.utils.MessageDigester.uuid;
import static org.neo4j.graphdb.Direction.INCOMING;
import static org.neo4j.graphdb.traversal.Evaluation.*;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 */
public abstract class AbstractUpdate extends Operator implements Evaluable {

    private Node root;

    protected AbstractUpdate(String... name) { super(name); }

    public OnQuestion onCalcQuestion() {
        return new Calc();
    }
    
    class Calc extends OnQuestion {
        @Override
        public void act(PFlow pf) throws Throwable {
            Pipe destination = Utils.getByREF(pf);
            IndexHits<Relationship> it = Order._.context(pf.getOP().getEndNode());
            try {
                Set<Relationship> param = new FastSet<Relationship>();
                for (Relationship r : it) {
                    param.add(r);
                }
                QCAVector v;
                while ((v = destination.take()) != null) {
                    execute(v, v.getClosest().getEndNode());
                }
            } finally {
                it.close();
            }
        }
    }

	private Iterator<Path> diff(Node start, final Node end) {
        return
            Traversal.description().depthFirst().
                uniqueness(Uniqueness.RELATIONSHIP_GLOBAL).
                evaluator(new Evaluator() {
                    @Override
                    public Evaluation evaluate(Path path) {
                        if (path.length() == 0)
                            return Evaluation.EXCLUDE_AND_CONTINUE;
                        if (!path.endNode().equals(path.lastRelationship().getEndNode()))
                            return EXCLUDE_AND_PRUNE;
                        if (path.lastRelationship().isType(REF._))
                            return EXCLUDE_AND_PRUNE;
                        if (Statements.relationshipType(path.lastRelationship()) == null)
                            return EXCLUDE_AND_PRUNE;
                        if (path.endNode().equals(end))
                            return INCLUDE_AND_PRUNE;
                        return EXCLUDE_AND_CONTINUE;
                    }
                }).traverse(start).iterator();
    }

    private void execute(QCAVector v, final Node x) throws Throwable {
        List<QCAVector> c = v.getContext();
        if (c != null) {
            for (QCAVector i : c) {
                execute(i, x);
            }
        } else {
            final Node n = v.getClosest().getEndNode();
            final Relationship r = n.getSingleRelationship(THE._, INCOMING);
            if (r != null) {
                AnimoGraph.execute(new GraphOperation<Void>() {
                    @Override
                    public Void execute() throws Throwable {
                        Node rev = THE._.getActualRevision(n);
                        Node rn = createNode();
                        Path path = diff(rev, x).next();
                        if (path != null) {
                            Relationship skip = path.relationships().iterator().next();
                            IndexHits<Relationship> it = Order._.queryDown(rev);
                            try{
                                for (Relationship i : it) {
                                    if (!i.equals(skip)) {
                                        copy(rn, i);
                                    }
                                }
                            } finally {
                                it.close();
                            }
                            process(rn, rev, x, path);
                        } else {
                            process(rn, rev, x);
                        }
                        Relationship rr = rev.createRelationshipTo(rn, RelationshipTypes.REV);
                        ARID.set(r, rr.getId());
                        ARID.set(n, rn.getId());
                        UUID.set(rr, uuid());
                        return null;
                    }
                });
                DependenciesTracking._.execute(null, r);
            }
        }
    }

    protected abstract void process(Node rn, Node rev, Node x);

    protected abstract void process(Node root, Node rev, Node x, Path diff);

}