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
package org.animotron.manipulator;

import org.animotron.graph.index.State;
import org.animotron.marker.AbstractMarker;
import org.animotron.marker.Marker;
import org.animotron.statement.Statement;
import org.animotron.statement.operator.Evaluable;
import org.jetlang.channels.Subscribable;
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
	public Subscribable<PFlow> onQuestion(Statement statement, Relationship op) {
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