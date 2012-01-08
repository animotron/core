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
import org.animotron.statement.query.GET;
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
public class MUL extends MathOperator {
	
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

}
