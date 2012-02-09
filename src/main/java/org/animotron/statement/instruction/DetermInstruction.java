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
package org.animotron.statement.instruction;

import org.animotron.graph.AnimoGraph;
import org.animotron.graph.GraphOperation;
import org.animotron.graph.Properties;
import org.animotron.manipulator.PFlow;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;

import static org.animotron.graph.RelationshipTypes.RESULT;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 *
 */
public abstract class DetermInstruction extends Instruction {

    public DetermInstruction(String... name) {
        super(name);
    }

    @Override
    protected void answered(final PFlow pf, final Relationship r) {
        Relationship res = AnimoGraph.execute(new GraphOperation<Relationship>() {
            @Override
            public Relationship execute() {
                Node sNode = pf.getVector().getQuestion().getEndNode();
                Relationship res = sNode.createRelationshipTo(r.getEndNode(), RESULT);
                Properties.RID.set(res, r.getId());
                //Properties.CID.set(res, pf.getLastContext().getId());
                return res;
            }
        });
        pf.sendAnswer(pf.getVector().answered(res));
    }

}