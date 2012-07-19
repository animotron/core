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

import org.animotron.expression.AbstractExpression;
import org.animotron.graph.GraphOperation;
import org.animotron.graph.builder.FastGraphBuilder;
import org.animotron.manipulator.OnQuestion;
import org.animotron.manipulator.PFlow;
import org.animotron.statement.operator.DEF;
import org.animotron.statement.operator.Prepare;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;

import java.util.Iterator;

import static org.animotron.graph.AnimoGraph.execute;
import static org.animotron.graph.RelationshipTypes.*;
import static org.neo4j.graphdb.Direction.OUTGOING;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public class VALUE extends AbstractValue implements Prepare {

    public final static VALUE _ = new VALUE();

    private VALUE() { super("value"); }

    protected VALUE(String... name) { super(name); }

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
                            Node n = r.getEndNode();
                            Node pend = null;
                            for (int i = 0; i < s.length(); i++) {
                                final String name = String.copyValueOf(new char[]{s.charAt(i)});
                                Relationship def = DEF._.get(name);
                                if (def == null) {
                                    def = new AbstractExpression(new FastGraphBuilder()) {
                                        @Override
                                        public void build() throws Throwable {
                                            builder.start(DEF._);
                                                builder._(name);
                                            builder.end();
                                        }
                                    };
                                }
                                Node end = def.getEndNode();
                                n.createRelationshipTo(end, CONSIST);
                                if (i == 0) {
                                    n.createRelationshipTo(end, FIRST);
                                } else {
                                    boolean f = true;
                                    Iterator<Relationship> it = pend.getRelationships(OUTGOING, NEXT).iterator();
                                    while (f && it.hasNext()) {
                                        f = it.next().getEndNode().equals(end);
                                    }
                                    if (f) {
                                        pend.createRelationshipTo(end, NEXT);
                                    }
                                    if (i == s.length() - 1) {
                                        n.createRelationshipTo(end, LAST);
                                    }
                                }
                                pend = end;
                            }
                            return null;
                        }
                    });
                }
            }
        }
    };
}