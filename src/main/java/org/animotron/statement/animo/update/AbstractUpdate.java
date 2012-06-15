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
import org.animotron.graph.index.Order;
import org.animotron.graph.serializer.DigestSerializer;
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

import java.io.IOException;
import java.util.Iterator;
import java.util.List;

import static org.animotron.graph.AnimoGraph.copy;
import static org.animotron.graph.AnimoGraph.createNode;
import static org.animotron.graph.Properties.HASH;
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
            QCAVector v;
            while ((v = destination.take()) != null) {
                execute(pf, v, v.getClosest().getEndNode());
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

    private void execute(final PFlow pf, QCAVector v, final Node x) throws Throwable {
        List<QCAVector> c = v.getContext();
        if (c != null) {
            for (QCAVector i : c) {
                execute(pf, i, x);
            }
        } else {
            final Node n = v.getClosest().getEndNode();
            final Relationship r = n.getSingleRelationship(DEF._, INCOMING);
            if (r != null) {
                AnimoGraph.execute(new GraphOperation<Void>() {
                    private void revision(Node rn, Node n, Relationship r) throws IOException {
                        Relationship rr = n.createRelationshipTo(rn, RelationshipTypes.REV);
                        AREV._.set(n, rn);
                        UUID.set(rr, uuid().toString());
                        HASH.set(rr, DigestSerializer._.serialize(rr));
                    }
                    @Override
                    public Void execute() throws Throwable {
                        Node rev = AREV._.actualNode(n);
                        Path path = diff(rev, x).next();
                        if (path != null) {
                            Node rn = createNode();
                            Relationship skip = path.relationships().iterator().next();
                            IndexHits<Relationship> it = Order._.queryDown(rev);
                            try {
                                for (Relationship i : it) {
                                    if (!i.equals(skip)) {
                                        copy(rn, i);
                                    }
                                }
                            } finally {
                                it.close();
                            }
                            revision(process(pf, rev, path), n, r);
                        } else if (n.equals(x)) {
                            revision(process(pf, rev), n, r);
                        }
                        return null;
                    }
                });
                DependenciesTracking._.execute(null, r);
            }
        }
    }

    protected abstract Node process(PFlow pf, Node n);

    protected abstract Node process(PFlow pf, Node n, Path diff);

}