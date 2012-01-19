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
package org.animotron.statement.query;

import org.animotron.manipulator.OnQuestion;
import org.animotron.manipulator.PFlow;

/**
 * Query operator 'IS'.
 * 
 * Return "true" if object has a given property
 * 
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 */
public class HAS extends AbstractQuery {

	public static final HAS _ = new HAS();

	private HAS() { super("?has"); }

	public OnQuestion onCalcQuestion() {
        return new Calc();
	}
	
	class Calc extends OnQuestion {
	
		@Override
        public void act(final PFlow pf) {
        }
    }
}