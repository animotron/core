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

import org.animotron.expression.JExpression;
import org.animotron.graph.GraphOperation;
import org.animotron.manipulator.OnQuestion;
import org.animotron.manipulator.PFlow;
import org.animotron.statement.operator.DEF;
import org.animotron.statement.operator.Prepare;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;

import static org.animotron.expression.JExpression._;
import static org.animotron.graph.AnimoGraph.execute;
import static org.animotron.graph.RelationshipTypes.*;

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
        return question;
    }

    private OnQuestion question = new OnQuestion() {
        @Override
        public void act(PFlow pf) throws Throwable {
            Relationship r = pf.getOP();
            final Node n = r.getEndNode();
            final Object o = reference(r);
            if (o instanceof String) {
                execute(new GraphOperation<Void>() {
                    @Override
                    public Void execute() throws Throwable {
                        String s = (String) o;
                        Node pn = null;
                        for (int i = 0; i < s.length(); i++) {
                            String name = String.copyValueOf(new char[]{s.charAt(i)});
                            Relationship def = DEF._.get(name);
                            if (def == null) {
                                def = new JExpression(_(DEF._, value(name)));
                            }
                            Node end = def.getEndNode();
                            n.createRelationshipTo(end, CONSIST);
                            if (i == 0) {
                                n.createRelationshipTo(end, FIRST);
                            } else {
                                pn.createRelationshipTo(end, NEXT);
                                if (i == s.length() - 1) {
                                    n.createRelationshipTo(end, LAST);
                                }
                            }
                            pn = n;
                        }
                        return null;
                    }
                });
            }
        }
    };
}