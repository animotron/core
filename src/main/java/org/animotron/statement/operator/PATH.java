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
package org.animotron.statement.operator;

import org.animotron.graph.index.Order;
import org.animotron.graph.traverser.It;
import org.animotron.io.Pipe;
import org.animotron.manipulator.OnQuestion;
import org.animotron.manipulator.PFlow;
import org.animotron.manipulator.QCAVector;
import org.animotron.statement.string.STRING;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Path;
import org.neo4j.graphdb.Relationship;

/**
 * Operation 'PATH'. Direct reference to 'the' instance.
 * 
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 */
public class PATH extends Operator implements Evaluable {

	public static final PATH _ = new PATH();

	private PATH() { super("//"); }

    public PATH(String... name) { super(name); }

    @Override
	public OnQuestion onCalcQuestion() {
		return new Calc();
    }

    class Calc extends OnQuestion {
	
		@Override
		public void act(final PFlow pf) throws Throwable {
            It it = new It(pf.getOP().getStartNode());
            byte[] hash = pf.getOpHash();

            if (!Utils.results(pf, hash)) {
                while (it.hasNext()) {

                }
                Pipe pipe = AN.getREFs(pf, pf.getVector());
                QCAVector r;
                while ((r = pipe.take()) != null) {
                    if (r.getQuestion().equals(pf.getVector().getQuestion()))
                        pf.sendAnswer(r);
                    else
                        pf.sendAnswer(r.getAnswer(), r);
                }
            }
        }
    }
	
}
