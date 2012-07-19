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
package org.animotron.statement.value;

import org.animotron.exception.AnimoException;
import org.animotron.expression.AbstractExpression;
import org.animotron.graph.GraphOperation;
import org.animotron.graph.builder.FastGraphBuilder;
import org.animotron.graph.index.AbstractIndex;
import org.animotron.manipulator.OnQuestion;
import org.animotron.manipulator.PFlow;
import org.animotron.statement.operator.AN;
import org.animotron.statement.operator.DEF;
import org.animotron.statement.operator.Prepare;
import org.animotron.statement.operator.REF;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.index.IndexManager;
import org.neo4j.index.bdbje.BerkeleyDbIndexImplementation;

import java.io.IOException;

import static org.animotron.graph.AnimoGraph.createNode;
import static org.animotron.graph.AnimoGraph.execute;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class VALUE extends AbstractValue implements Prepare {

    public final static VALUE _ = new VALUE();

    private VALUE() { super("value"); }

    protected VALUE(String... name) { super(name); }

    private AbstractIndex<Node> value = new AbstractIndex<Node>(name()) {
        @Override
        public void init(IndexManager index) {
            init(index.forNodes(name, BerkeleyDbIndexImplementation.DEFAULT_CONFIG));
        }
    };

    public void init(IndexManager index) {
        value.init(index);
	}

	public void add(Node n, Object reference) {
        value.add(n, reference);
	}

	public Node get(Object name) {
        return value.get(name);
	}

    @Override
    protected Node createChild(Object reference, boolean ready, boolean ignoreNotFound) throws AnimoException {
        Node child = super.createChild(reference, ready, ignoreNotFound);
        add(child, reference);
        return child;
    }

    @Override
    public OnQuestion onPrepareQuestion() {
        return new Prepare();
    }

    class Prepare extends OnQuestion {
		public boolean needAnswer() {
			return false;
		}

    	@Override
        public void act(PFlow pf) throws Throwable {
            final Relationship r = pf.getOP();
            Object o = reference(r);
            if (o instanceof String) {
                final String s = (String) o;
                if (s.length() > 1) {
                    execute(new GraphOperation<Void>() {
                        @Override
                        public Void execute() throws Throwable {
                            Relationship x = new AbstractExpression(new FastGraphBuilder()) {

                                private void step(final String value, final int i) throws AnimoException, IOException {
                                    if (i >= 0) {
                                        Relationship def = new AbstractExpression(new FastGraphBuilder()) {
                                            @Override
                                            public void build() throws Throwable {
                                                builder.start(DEF._);
                                                    builder._(String.copyValueOf(new char[]{value.charAt(i)}));
                                                builder.end();
                                            }
                                        };
                                        builder.start(AN._);
                                            builder._(REF._, def.getEndNode());
                                            step(value, i-1);
                                        builder.end();
                                    }
                                }

                                @Override
                                public void build() throws Throwable {
                                    builder.start(DEF._);
                                        step(s, s.length()-1);
                                    builder.end();
                                }

                            };

                            Node proxy = createNode();

                            r.getEndNode().createRelationshipTo(proxy, AN._);
                            proxy.createRelationshipTo(x.getEndNode(), REF._);

                            return null;
                        }
                    });
                }
            }
        }
    };
}