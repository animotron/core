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
package org.animotron.operator.query;

import org.animotron.manipulator.OnQuestion;
import org.animotron.manipulator.PFlow;
import org.animotron.operator.AbstarctOperator;
import org.animotron.operator.Evaluable;

/**
 * Query operator 'self'.
 * 
 * @author <a href="mailto:shabanovd@gmail.com">Dmitriy Shabanov</a>
 * @author <a href="mailto:gazdovsky@gmail.com">Evgeny Gazdovsky</a>
 */
public class SELF extends AbstarctOperator implements Evaluable {
	
	public static final SELF _ = new SELF();
	
	private SELF() { super("self", "animo/query/self"); }

	@Override
	public OnQuestion onCalcQuestion() {
		return new OnQuestion() {
			@Override
			public void onMessage(final PFlow pf) {
				System.out.println("SELF '"+name(pf.getOP())+"' op = "+pf.getOP());
				
				System.out.println("path = "+pf.getFlowPath());
				
				pf.done();
			}
		};
	}
	
}
