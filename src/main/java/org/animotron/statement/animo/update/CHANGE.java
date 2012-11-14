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
import org.animotron.graph.index.AShift;
import org.animotron.graph.index.Order;
import org.animotron.io.Pipe;
import org.animotron.manipulator.DependenciesTracking;
import org.animotron.manipulator.OnQuestion;
import org.animotron.manipulator.PFlow;
import org.animotron.manipulator.QCAVector;
import org.animotron.statement.Statement;
import org.animotron.statement.Statements;
import org.animotron.statement.operator.*;
import org.animotron.statement.query.GET;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.index.IndexHits;

import java.util.List;
import java.util.Set;

import static org.animotron.graph.Properties.*;
import static org.animotron.graph.RelationshipTypes.SHIFT;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 */
public class CHANGE extends Operator implements Evaluable {

    public static final CHANGE _ = new CHANGE();

	private CHANGE() {super("change");}

    @Override
    public OnQuestion onCalcQuestion() {
        return new Calc();
    }

    private class Calc extends OnQuestion {

        @Override
        public void act(final PFlow pf) throws Throwable {

            Relationship op = null;
            Relationship np = null;
            IndexHits<Relationship> it = Order._.queryDown(pf.getOPNode());
            try {
                int i = 0;
                while (it.hasNext()) {
                    Relationship r = it.next();
                    if (i > 0 && !r.isType(REF._)) {
                        if (np == null) {
                            np = r;
                        } else if (op == null) {
                            op = np;
                            np = r;
                            break;
                        }
                    }
                    i++;
                }
            } finally {
                it.close();
            }
            
            process(pf, op, np);
        }

        private void process(final PFlow pf, final Relationship op, final Relationship np) throws Throwable {
            AnimoGraph.execute(new GraphOperation<Void>() {
                @Override
                public Void execute() throws Throwable {
                    Pipe pipe = Utils.getByREF(pf, pf.getVector());
                    QCAVector v;
                    while ((v = pipe.take()) != null) {
//                    	System.out.println("process "+v);
                        process(pf, v, op, np);
//                    	System.out.println("end process "+v);
                    }
                    return null;
                }
            });
        }

        private void process(PFlow pf, QCAVector v, Relationship op, Relationship np) throws Throwable {
            long rid = np.getId();
            long uid = pf.getOP().getId();
            Relationship c = v.getClosest();
            Statement q = Statements.relationshipType(v.getQuestion());
            IndexHits<Relationship> it = Order._.queryDown(c.isType(DEF._) || c.isType(REF._) && !(q instanceof GET) ? c.getEndNode() : c.getStartNode());
            try {
                if (it.hasNext()) {
                    while (it.hasNext()) {
                        Relationship r = it.next();
                        if (op == null || r.getEndNode().equals(op.getEndNode())) {
                            process(pf, r.getStartNode(), np, v, uid, rid);
                        }
                    }
                } else {
                    process(pf, c.getEndNode(), np, v, uid, rid);
                }
            } finally {
                it.close();
            }
        }

        private void process(PFlow pf, Node a, Relationship np, QCAVector v, long uid, long rid) throws Throwable {
        	FastSet<Relationship> set = FastSet.newInstance();
        	try {
	            for (Relationship i : def(set, v)) {
	                long def = i.getId();
	                Node s = a;
	                Node n = np.getEndNode();
	                Relationship ashift = AShift._.get(s, def);
	                if (ashift == null) {
	                    ashift = a.createRelationshipTo(n, ASHIFT._);
	                } else {
	                    s = ashift.getEndNode();
	                    AShift._.remove(ashift, def);
	                    ashift.delete();
	                    ashift = a.createRelationshipTo(n, ASHIFT._);
	                }
	                Relationship shift = s.createRelationshipTo(n, SHIFT);
	                UID.set(ashift, uid);
	                UID.set(shift, uid);
	                DEFID.set(ashift, def);
	                DEFID.set(shift, def);
	                RID.set(ashift, rid);
	                RID.set(shift, rid);
	                AShift._. add(ashift, def);
	                HASH.remove(i);
	                DependenciesTracking._.execute(pf.getController(), i);
	            }
        	} finally {
        		FastSet.recycle(set);
        	}
        }

        private Set<Relationship> def(Set<Relationship> set, QCAVector v) {
            List<QCAVector> context = v.getContext();
            if (context == null) {
                Node def = v.getClosestDefEndNode();
                if (def != null) {
                    set.add(DEF._.get(def));
                }
            } else {
                for (QCAVector i : context) {
                    def(set, i);
                }
            }
            return set;
        }
    }

}
