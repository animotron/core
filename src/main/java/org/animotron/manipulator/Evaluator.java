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
package org.animotron.manipulator;

import org.animotron.graph.index.State;
import org.animotron.marker.AbstractMarker;
import org.animotron.marker.Marker;
import org.animotron.statement.Statement;
import org.animotron.statement.operator.Evaluable;
import org.neo4j.graphdb.Relationship;

/**
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 *
 */
public class Evaluator extends StatementManipulator {

	public static Evaluator _ = new Evaluator();
	
	private Evaluator() {}
	
	@Override
	public boolean canGo(Statement statement) {
		return statement instanceof Evaluable;
	}

	@Override
	public OnQuestion onQuestion(Statement statement, Relationship op) {
		return ((Evaluable) statement).onCalcQuestion();
	}

	@Override
	public Marker marker() {
		return CalcMarker._;
	}
	
	private static class CalcMarker extends AbstractMarker {
		
		private static final Marker _ = new CalcMarker(); 
		private CalcMarker() {super(State.CALC);}

		@Override
		public Manipulator manipulator() {
			return Evaluator._;
		}
	}
}