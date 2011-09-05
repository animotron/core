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
package org.animotron.statement.query;

import org.animotron.manipulator.OnQuestion;
import org.animotron.manipulator.PFlow;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;

import java.util.Set;

import static org.animotron.graph.RelationshipTypes.REF;
import static org.neo4j.graphdb.Direction.OUTGOING;

/**
 * Query operator 'ALL'.
 * 
 * Return nothing (if no USE) or 'all' USE. 
 * 
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 */
public class ALL extends AbstractQuery {
	
	public static final ALL _ = new ALL();
	
	private ALL() { super("all"); }

    public OnQuestion onCalcQuestion() {
        return question;
    }

    private OnQuestion question = new OnQuestion() {
        @Override
        public void onMessage(final PFlow pf) {
            final Node n = pf.getOP().getEndNode();
            Relationship ref = n.getSingleRelationship( REF, OUTGOING );

            Node node = ref.getEndNode();

			//System.out.println("ALL **************************");

			Set<Node>[] lists = getUSEs(node, pf.getStartOP());
			Set<Node> uses = lists[1];
			Set<Node> directed = lists[2];
			
			boolean underUSE = false;
			if (directed != null && directed.size() == 1) { 
				underUSE = true;
				node = directed.iterator().next();
			}

			if (underUSE && filtering(pf, node, uses))
				pf.sendAnswer( createResultInMemory( n, getThe(node) ) );

	        for (Relationship tdR : td_IS.traverse(node).relationships()) {
	            //System.out.println("ALL get next "+tdR+" ["+tdR.getStartNode()+"]");
	            Node res = tdR.getStartNode();
	            if (filtering(pf, res, uses)) {
	                pf.sendAnswer( createResultInMemory( n, getThe(res) ) );
	            }
	        }

            pf.done();
        }

    };

}