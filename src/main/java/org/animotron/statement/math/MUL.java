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
package org.animotron.statement.math;

import javolution.util.FastList;

import org.animotron.graph.Properties;
import org.animotron.graph.AnimoGraph;
import org.animotron.graph.GraphOperation;
import org.animotron.graph.index.Order;
import org.animotron.manipulator.OnQuestion;
import org.animotron.manipulator.PFlow;
import org.animotron.manipulator.QCAVector;
import org.animotron.statement.operator.AN;
import org.animotron.statement.operator.Prepare;
import org.jetlang.channels.Subscribable;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.index.IndexHits;

import static org.animotron.graph.RelationshipTypes.TRI;

/**
 * Math instruction 'MULT'. (aka multiplication)
 * 
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 */
public class MUL extends MathOperator implements Prepare {
	
	public static final MUL _ = new MUL();
	
	private MUL() { super("*"); }

    @Override
    protected Number execute(Number a, Number b) {
        if (a instanceof Long && b instanceof Long) {
            return a.longValue() * b.longValue();
        } else {
            return a.doubleValue() * b.doubleValue();
        }
    }

    @Override
    protected Number execute(Number a) {
        return a;
    }

	@Override
	public Subscribable<PFlow> onPrepareQuestion() {
		return prepare;
	}


    private OnQuestion prepare = new OnQuestion() {
    	@Override
    	public void onMessage(final PFlow pf) {
    		final FastList<Node> thes = FastList.newInstance();
    		IndexHits<Relationship> hits = Order.context(pf.getOPNode());
    		try {
    			for (Relationship r : hits) {
    				if (!r.isType(AN._)) {
    					pf.done();
    					return;
    				}
    				
    				for (QCAVector v : AN.getREFs(pf, new QCAVector(r))) {
    					thes.add(v.getClosest().getEndNode());
    				}
    				
    				if (thes.size() > 2) {
    					pf.done();
    					return;
    				}
    			}
    			
				if (thes.size() == 2) {

	    			AnimoGraph.execute(new GraphOperation<Void>() {
	
						@Override
						public Void execute() throws Exception {
							Relationship r = thes.get(0).createRelationshipTo(thes.get(1), TRI);
							Properties.TYPE.set(r, MUL._.name());
							Properties.TO_NODE.set(r, pf.getOP().getStartNode().getId());
							return null;
						}
	    				
	    			});
				}
    			
    		} finally {
    			hits.close();
    			FastList.recycle(thes);
    		}
    		pf.done();
    	}
    };
}
